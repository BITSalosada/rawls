package org.broadinstitute.dsde.rawls.workspace

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import org.broadinstitute.dsde.rawls.{RawlsException, RawlsExceptionWithErrorReport}
import slick.jdbc.TransactionIsolation
import org.broadinstitute.dsde.rawls.dataaccess._
import org.broadinstitute.dsde.rawls.dataaccess.slick._
import org.broadinstitute.dsde.rawls.expressions._
import org.broadinstitute.dsde.rawls.genomics.GenomicsService
import org.broadinstitute.dsde.rawls.jobexec.MethodConfigResolver
import org.broadinstitute.dsde.rawls.metrics.RawlsInstrumented
import org.broadinstitute.dsde.rawls.model.Attributable.AttributeMap
import org.broadinstitute.dsde.rawls.model.AttributeUpdateOperations._
import org.broadinstitute.dsde.rawls.model.ExecutionJsonSupport.{ActiveSubmissionFormat, SubmissionListResponseFormat, SubmissionReportFormat, SubmissionStatusResponseFormat, SubmissionValidationReportFormat, WorkflowCostFormat, WorkflowOutputsFormat, WorkflowQueueStatusByUserResponseFormat, WorkflowQueueStatusResponseFormat}
import org.broadinstitute.dsde.rawls.model.MethodRepoJsonSupport.AgoraEntityFormat
import org.broadinstitute.dsde.rawls.model.WorkflowFailureModes.WorkflowFailureMode
import org.broadinstitute.dsde.rawls.model.WorkspaceACLJsonSupport.{WorkspaceACLFormat, WorkspaceACLUpdateResponseListFormat, WorkspaceCatalogFormat, WorkspaceCatalogUpdateResponseListFormat}
import org.broadinstitute.dsde.rawls.model.WorkspaceAccessLevels._
import org.broadinstitute.dsde.rawls.model.WorkspaceJsonSupport._
import org.broadinstitute.dsde.rawls.model._
import org.broadinstitute.dsde.rawls.user.UserService
import org.broadinstitute.dsde.rawls.util._
import org.broadinstitute.dsde.rawls.webservice.PerRequest
import org.broadinstitute.dsde.rawls.webservice.PerRequest._
import org.joda.time.DateTime
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.headers.OAuth2BearerToken
import akka.http.scaladsl.model.{StatusCodes, Uri}
import com.google.api.services.storage.model.StorageObject
import org.broadinstitute.dsde.rawls.model.WorkflowStatuses.WorkflowStatus
import org.broadinstitute.dsde.workbench.model.{WorkbenchEmail, WorkbenchException, WorkbenchGroupName}
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import cats.implicits._
import io.opencensus.trace.Span
import io.opencensus.scala.Tracing._
import io.opencensus.trace.{AttributeValue => OpenCensusAttributeValue}
import org.broadinstitute.dsde.rawls.util.OpenCensusDBIOUtils._
import io.opencensus.trace.{Span, Status}



/**
 * Created by dvoet on 4/27/15.
 */

object WorkspaceService {
  def constructor(dataSource: SlickDataSource, methodRepoDAO: MethodRepoDAO, cromiamDAO: ExecutionServiceDAO, executionServiceCluster: ExecutionServiceCluster, execServiceBatchSize: Int, methodConfigResolver: MethodConfigResolver, gcsDAO: GoogleServicesDAO, samDAO: SamDAO, notificationDAO: NotificationDAO, userServiceConstructor: UserInfo => UserService, genomicsServiceConstructor: UserInfo => GenomicsService, maxActiveWorkflowsTotal: Int, maxActiveWorkflowsPerUser: Int, workbenchMetricBaseName: String, submissionCostService: SubmissionCostService, config: WorkspaceServiceConfig)(userInfo: UserInfo)(implicit executionContext: ExecutionContext) = {
    new WorkspaceService(userInfo, dataSource, methodRepoDAO, cromiamDAO, executionServiceCluster, execServiceBatchSize, methodConfigResolver, gcsDAO, samDAO, notificationDAO, userServiceConstructor, genomicsServiceConstructor, maxActiveWorkflowsTotal, maxActiveWorkflowsPerUser, workbenchMetricBaseName, submissionCostService, config)
  }

  val SECURITY_LABEL_KEY = "security"
  val HIGH_SECURITY_LABEL = "high"
  val LOW_SECURITY_LABEL = "low"

  private[workspace] def extractOperationIdsFromCromwellMetadata(metadataJson: JsObject): Iterable[String] = {
    case class Call(jobId: Option[String])
    case class OpMetadata(calls: Option[Map[String, Seq[Call]]])
    implicit val callFormat = jsonFormat1(Call)
    implicit val opMetadataFormat = jsonFormat1(OpMetadata)

    for {
      calls <- metadataJson.convertTo[OpMetadata].calls.toList // toList on the Option makes the compiler like the for comp
      call <- calls.values.flatten
      jobId <- call.jobId
    } yield jobId
  }

}

final case class WorkspaceServiceConfig(trackDetailedSubmissionMetrics: Boolean, workspaceBucketNamePrefix: String)

class WorkspaceService(protected val userInfo: UserInfo, val dataSource: SlickDataSource, val methodRepoDAO: MethodRepoDAO, cromiamDAO: ExecutionServiceDAO, executionServiceCluster: ExecutionServiceCluster, execServiceBatchSize: Int, val methodConfigResolver: MethodConfigResolver, protected val gcsDAO: GoogleServicesDAO, val samDAO: SamDAO, notificationDAO: NotificationDAO, userServiceConstructor: UserInfo => UserService, genomicsServiceConstructor: UserInfo => GenomicsService, maxActiveWorkflowsTotal: Int, maxActiveWorkflowsPerUser: Int, override val workbenchMetricBaseName: String, submissionCostService: SubmissionCostService, config: WorkspaceServiceConfig)(implicit protected val executionContext: ExecutionContext)
  extends RoleSupport with LibraryPermissionsSupport with FutureSupport with MethodWiths with UserWiths with LazyLogging with RawlsInstrumented with JsonFilterUtils {

  import dataSource.dataAccess.driver.api._

  def CreateWorkspace(workspace: WorkspaceRequest, parentSpan: Span = null) = createWorkspace(workspace, parentSpan)
  def GetWorkspace(workspaceName: WorkspaceName, params: WorkspaceFieldSpecs, parentSpan: Span) = getWorkspace(workspaceName, params, parentSpan)
  def DeleteWorkspace(workspaceName: WorkspaceName) = deleteWorkspace(workspaceName)
  def UpdateWorkspace(workspaceName: WorkspaceName, operations: Seq[AttributeUpdateOperation]) = updateWorkspace(workspaceName, operations)
  def UpdateLibraryAttributes(workspaceName: WorkspaceName, operations: Seq[AttributeUpdateOperation]) = updateLibraryAttributes(workspaceName, operations)
  def ListWorkspaces(params: WorkspaceFieldSpecs, parentSpan: Span) = listWorkspaces(params, parentSpan)
  def ListAllWorkspaces = listAllWorkspaces()
  def GetTags(query: Option[String]) = getTags(query)
  def AdminListWorkspacesWithAttribute(attributeName: AttributeName, attributeValue: AttributeValue) = asFCAdmin { listWorkspacesWithAttribute(attributeName, attributeValue) }
  def CloneWorkspace(sourceWorkspace: WorkspaceName, destWorkspace: WorkspaceRequest) = cloneWorkspace(sourceWorkspace, destWorkspace)
  def GetACL(workspaceName: WorkspaceName) = getACL(workspaceName)
  def UpdateACL(workspaceName: WorkspaceName, aclUpdates: Set[WorkspaceACLUpdate], inviteUsersNotFound: Boolean) = updateACL(workspaceName, aclUpdates, inviteUsersNotFound)
  def SendChangeNotifications(workspaceName: WorkspaceName) = sendChangeNotifications(workspaceName)
  def GetCatalog(workspaceName: WorkspaceName) = getCatalog(workspaceName)
  def UpdateCatalog(workspaceName: WorkspaceName, catalogUpdates: Seq[WorkspaceCatalog]) = updateCatalog(workspaceName, catalogUpdates)
  def LockWorkspace(workspaceName: WorkspaceName) = lockWorkspace(workspaceName)
  def UnlockWorkspace(workspaceName: WorkspaceName) = unlockWorkspace(workspaceName)
  def CheckBucketReadAccess(workspaceName: WorkspaceName) = checkBucketReadAccess(workspaceName)
  def CheckSamActionWithLock(workspaceName: WorkspaceName, requiredAction: SamResourceAction) = checkSamActionWithLock(workspaceName, requiredAction)
  def GetBucketUsage(workspaceName: WorkspaceName) = getBucketUsage(workspaceName)
  def GetBucketOptions(workspaceName: WorkspaceName) = getBucketOptions(workspaceName)
  //def UpdateBucketOptions(workspaceName: WorkspaceName, bucketOptions: WorkspaceBucketOptions) = updateBucketOptions(workspaceName, bucketOptions)
  def GetAccessInstructions(workspaceName: WorkspaceName) = getAccessInstructions(workspaceName)

  def CreateEntity(workspaceName: WorkspaceName, entity: Entity) = createEntity(workspaceName, entity)
  def GetEntity(workspaceName: WorkspaceName, entityType: String, entityName: String) = getEntity(workspaceName, entityType, entityName)
  def UpdateEntity(workspaceName: WorkspaceName, entityType: String, entityName: String, operations: Seq[AttributeUpdateOperation]) = updateEntity(workspaceName, entityType, entityName, operations)
  def DeleteEntities(workspaceName: WorkspaceName, entities: Seq[AttributeEntityReference]) = deleteEntities(workspaceName, entities)
  def RenameEntity(workspaceName: WorkspaceName, entityType: String, entityName: String, newName: String) = renameEntity(workspaceName, entityType, entityName, newName)
  def EvaluateExpression(workspaceName: WorkspaceName, entityType: String, entityName: String, expression: String) = evaluateExpression(workspaceName, entityType, entityName, expression)
  def GetEntityTypeMetadata(workspaceName: WorkspaceName) = entityTypeMetadata(workspaceName)
  def ListEntities(workspaceName: WorkspaceName, entityType: String) = listEntities(workspaceName, entityType)
  def QueryEntities(workspaceName: WorkspaceName, entityType: String, query: EntityQuery) = queryEntities(workspaceName, entityType, query)
  def CopyEntities(entityCopyDefinition: EntityCopyDefinition, uri:Uri, linkExistingEntities: Boolean) = copyEntities(entityCopyDefinition, uri, linkExistingEntities)
  def BatchUpsertEntities(workspaceName: WorkspaceName, entityUpdates: Seq[EntityUpdateDefinition]) = batchUpdateEntities(workspaceName, entityUpdates, true)
  def BatchUpdateEntities(workspaceName: WorkspaceName, entityUpdates: Seq[EntityUpdateDefinition]) = batchUpdateEntities(workspaceName, entityUpdates, false)

  def CreateMethodConfiguration(workspaceName: WorkspaceName, methodConfiguration: MethodConfiguration) = createMethodConfiguration(workspaceName, methodConfiguration)
  def RenameMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String, newName: MethodConfigurationName) = renameMethodConfiguration(workspaceName, methodConfigurationNamespace, methodConfigurationName, newName)
  def DeleteMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String) = deleteMethodConfiguration(workspaceName, methodConfigurationNamespace, methodConfigurationName)
  def GetMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String) = getMethodConfiguration(workspaceName, methodConfigurationNamespace, methodConfigurationName)
  def OverwriteMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String, newMethodConfiguration: MethodConfiguration) = overwriteMethodConfiguration(workspaceName, methodConfigurationNamespace, methodConfigurationName, newMethodConfiguration)
  def UpdateMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String, newMethodConfiguration: MethodConfiguration) = updateMethodConfiguration(workspaceName, methodConfigurationNamespace, methodConfigurationName, newMethodConfiguration)
  def CopyMethodConfiguration(methodConfigNamePair: MethodConfigurationNamePair) = copyMethodConfiguration(methodConfigNamePair)
  def CopyMethodConfigurationFromMethodRepo(query: MethodRepoConfigurationImport) = copyMethodConfigurationFromMethodRepo(query)
  def CopyMethodConfigurationToMethodRepo(query: MethodRepoConfigurationExport) = copyMethodConfigurationToMethodRepo(query)
  def ListAgoraMethodConfigurations(workspaceName: WorkspaceName) = listAgoraMethodConfigurations(workspaceName)
  def ListMethodConfigurations(workspaceName: WorkspaceName) = listMethodConfigurations(workspaceName)
  def CreateMethodConfigurationTemplate( methodRepoMethod: MethodRepoMethod ) = createMethodConfigurationTemplate(methodRepoMethod)
  def GetMethodInputsOutputs(userInfo: UserInfo, methodRepoMethod: MethodRepoMethod ) = getMethodInputsOutputs(userInfo, methodRepoMethod)
  def GetAndValidateMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String) = getAndValidateMethodConfiguration(workspaceName, methodConfigurationNamespace, methodConfigurationName)
  def GetGenomicsOperationV2(workflowId: String, operationId: List[String]) = getGenomicsOperationV2(workflowId, operationId)

  def ListSubmissions(workspaceName: WorkspaceName) = listSubmissions(workspaceName)
  def CountSubmissions(workspaceName: WorkspaceName) = countSubmissions(workspaceName)
  def CreateSubmission(workspaceName: WorkspaceName, submission: SubmissionRequest) = createSubmission(workspaceName, submission)
  def ValidateSubmission(workspaceName: WorkspaceName, submission: SubmissionRequest) = validateSubmission(workspaceName, submission)
  def GetSubmissionStatus(workspaceName: WorkspaceName, submissionId: String) = getSubmissionStatus(workspaceName, submissionId)
  def AbortSubmission(workspaceName: WorkspaceName, submissionId: String) = abortSubmission(workspaceName, submissionId)
  def GetWorkflowOutputs(workspaceName: WorkspaceName, submissionId: String, workflowId: String) = workflowOutputs(workspaceName, submissionId, workflowId)
  def GetWorkflowMetadata(workspaceName: WorkspaceName, submissionId: String, workflowId: String, metadataParams: MetadataParams) = workflowMetadata(workspaceName, submissionId, workflowId, metadataParams)
  def GetWorkflowCost(workspaceName: WorkspaceName, submissionId: String, workflowId: String) = workflowCost(workspaceName, submissionId, workflowId)
  def WorkflowQueueStatus = workflowQueueStatus()

  def AdminListAllActiveSubmissions = asFCAdmin { listAllActiveSubmissions() }
  def AdminAbortSubmission(workspaceName: WorkspaceName, submissionId: String) = adminAbortSubmission(workspaceName,submissionId)
  def AdminWorkflowQueueStatusByUser = adminWorkflowQueueStatusByUser()

  def createWorkspace(workspaceRequest: WorkspaceRequest, parentSpan: Span = null): Future[Workspace] =
    traceWithParent("withAttributeNamespaceCheck", parentSpan)( s1 => withAttributeNamespaceCheck(workspaceRequest) {
      traceWithParent("withNewWorkspaceContext", s1)( s2 => dataSource.inTransaction({ dataAccess =>
        withNewWorkspaceContext(workspaceRequest, dataAccess, s2) { workspaceContext =>
          DBIO.successful(workspaceContext.workspace)
        }
      }, TransactionIsolation.ReadCommitted)) // read committed to avoid deadlocks on workspace attr scratch table
    })

  /** Returns the Set of legal field names supplied by the user, trimmed of whitespace.
    * Throws an error if the user supplied an unrecognized field name.
    * Legal field names are any member of `WorkspaceResponse`, `WorkspaceDetails`,
    * or any arbitrary key starting with "workspace.attributes."
    *
    * @param params the raw strings supplied by the user
    * @return the set of field names to be included in the response
    */
  def validateParams(params: WorkspaceFieldSpecs, default: Set[String]): Set[String] = {
    // be lenient to whitespace, e.g. some user included spaces in their delimited string ("one, two, three")
    val args = params.fields.getOrElse(default).map(_.trim)
    // did the user specify any fields that we don't know about?
    // include custom leniency here for attributes: we can't validate attribute names because they are arbitrary,
    // so allow any field that starts with "workspace.attributes."
    val unrecognizedFields: Set[String] = args.diff(default).filter(!_.startsWith("workspace.attributes."))
    if (unrecognizedFields.nonEmpty) {
      throw new RawlsException(s"Unrecognized field names: ${unrecognizedFields.toList.sorted.mkString(", ")}")
    }
    args
  }

  def getWorkspace(workspaceName: WorkspaceName, params: WorkspaceFieldSpecs, parentSpan: Span = null): Future[PerRequestMessage] = {
    val span = startSpanWithParent("optionsProcessing", parentSpan)

    // validate the inbound parameters
    val options = Try(validateParams(params, WorkspaceFieldNames.workspaceResponseFieldNames)) match {
      case Success(opts) => opts
      case Failure(ex) => throw new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, ex))
    }

    // dummy function that returns a Future(None)
    def noFuture = Future.successful(None)

    // if user requested the entire attributes map, or any individual attributes, retrieve attributes.
    val attrSpecs = WorkspaceAttributeSpecs(
      options.contains("workspace.attributes"),
      options.filter(_.startsWith("workspace.attributes."))
        .map(str => AttributeName.fromDelimitedName(str.replaceFirst("workspace.attributes.",""))).toList
    )
    span.setStatus(Status.OK)
    span.end()

    dataSource.inTransaction { dataAccess =>
      traceDBIOWithParent("withWorkspaceContext", parentSpan)(s1 => withWorkspaceContext(workspaceName, dataAccess, Option(attrSpecs)) { workspaceContext =>
        traceDBIOWithParent("requireAccess", s1)(s2 => requireAccess(workspaceContext.workspace, SamWorkspaceActions.read) {

          // maximum access level is required to calculate canCompute and canShare. Therefore, if any of
          // accessLevel, canCompute, canShare is specified, we have to get it.
          def accessLevelFuture(): Future[WorkspaceAccessLevels.WorkspaceAccessLevel] =
            if (options.contains("accessLevel") || options.contains("canCompute") || options.contains("canShare")) {
              getMaximumAccessLevel(workspaceContext.workspaceId.toString)
            } else {
              Future.successful(WorkspaceAccessLevels.NoAccess)
            }

          // determine whether or not to retrieve attributes
          val useAttributes = options.contains("workspace") || attrSpecs.all || attrSpecs.attrsToSelect.nonEmpty

          traceDBIOWithParent("accessLevelFuture", s2)(s3 => DBIO.from(accessLevelFuture())) flatMap { accessLevel =>
            // we may have calculated accessLevel because canShare/canCompute needs it;
            // but if the user didn't ask for it, don't return it
            val optionalAccessLevelForResponse = if (options.contains("accessLevel")) { Option(accessLevel) } else { None }

            // determine which functions to use for the various part of the response
            def bucketOptionsFuture(): Future[Option[WorkspaceBucketOptions]] = if (options.contains("bucketOptions")) {
              traceWithParent("getBucketDetails",s2)(_ =>  gcsDAO.getBucketDetails(workspaceContext.workspace.bucketName, RawlsBillingProjectName(workspaceContext.workspace.namespace)).map(Option(_)))
            } else {
              noFuture
            }
            def canComputeFuture(): Future[Option[Boolean]] = if (options.contains("canCompute")) {
              traceWithParent("getUserComputePermissions",s2)(_ =>  getUserComputePermissions(workspaceContext.workspaceId.toString, accessLevel).map(Option(_)))
            } else {
              noFuture
            }
            def canShareFuture(): Future[Option[Boolean]] = if (options.contains("canShare")) {
              //convoluted but accessLevel for both params because user could at most share with their own access level
              traceWithParent("getUserSharePermissions",s2)(_ =>  getUserSharePermissions(workspaceContext.workspaceId.toString, accessLevel, accessLevel).map(Option(_)))
            } else {
              noFuture
            }
            def catalogFuture(): Future[Option[Boolean]] = if (options.contains("catalog")) {
              traceWithParent("getUserCatalogPermissions",s2)(_ =>  getUserCatalogPermissions(workspaceContext.workspaceId.toString).map(Option(_)))
            } else {
              noFuture
            }

            def ownersFuture(): Future[Option[Set[String]]] = if (options.contains("owners")) {
              traceWithParent("getWorkspaceOwners",s2)(_ =>  getWorkspaceOwners(workspaceContext.workspaceId.toString).map(_.map(_.value)).map(Option(_)))
            } else {
              noFuture
            }

            def workspaceAuthorizationDomainFuture(): Future[Option[Set[ManagedGroupRef]]] = if (options.contains("workspace.authorizationDomain") || options.contains("workspace")) {
              traceWithParent("loadResourceAuthDomain",s2)(_ =>  loadResourceAuthDomain(SamResourceTypeNames.workspace, workspaceContext.workspace.workspaceId, userInfo).map(Option(_)))
            } else {
              noFuture
            }

            def workspaceSubmissionStatsFuture(): slick.ReadAction[Option[WorkspaceSubmissionStats]] = if (options.contains("workspaceSubmissionStats")) {
              getWorkspaceSubmissionStats(workspaceContext, dataAccess).map(Option(_))
            } else {
              DBIO.from(noFuture)
            }

            //run these futures in parallel. this is equivalent to running the for-comp with the futures already defined and running
            val futuresInParallel = (
              catalogFuture(),
              canShareFuture(),
              canComputeFuture(),
              ownersFuture(),
              workspaceAuthorizationDomainFuture(),
              bucketOptionsFuture()
            ).tupled

            for {
              (canCatalog, canShare, canCompute, owners, authDomain, bucketDetails) <- DBIO.from(futuresInParallel)
              stats <- traceDBIOWithParent("workspaceSubmissionStatsFuture", s2)( _ => workspaceSubmissionStatsFuture())
            } yield {
              // post-process JSON to remove calculated-but-undesired keys
              val workspaceResponse = WorkspaceResponse(optionalAccessLevelForResponse, canShare, canCompute, canCatalog, WorkspaceDetails.fromWorkspaceAndOptions(workspaceContext.workspace, authDomain, useAttributes), stats, bucketDetails, owners)
              val filteredJson = deepFilterJsObject(workspaceResponse.toJson.asJsObject, options)
              RequestComplete(StatusCodes.OK, filteredJson)
            }
          }
        })
      })
    }
  }

  def getBucketOptions(workspaceName: WorkspaceName): Future[PerRequestMessage] = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContext(workspaceName, dataAccess) { workspaceContext =>
        requireAccess(workspaceContext.workspace, SamWorkspaceActions.read) {
          DBIO.from(gcsDAO.getBucketDetails(workspaceContext.workspace.bucketName, RawlsBillingProjectName(workspaceContext.workspace.namespace))) map { details =>
            RequestComplete(StatusCodes.OK, details)
          }
        }
      }
    }
  }

  private def loadResourceAuthDomain(resourceTypeName: SamResourceTypeName, resourceId: String, userInfo: UserInfo): Future[Set[ManagedGroupRef]] = {
    samDAO.getResourceAuthDomain(resourceTypeName, resourceId, userInfo).map(_.map(g => ManagedGroupRef(RawlsGroupName(g))).toSet)
  }

  def getUserComputePermissions(workspaceId: String, userAccessLevel: WorkspaceAccessLevel): Future[Boolean] = {
    if(userAccessLevel >= WorkspaceAccessLevels.Owner) Future.successful(true)
    else samDAO.userHasAction(SamResourceTypeNames.workspace, workspaceId, SamWorkspaceActions.compute, userInfo)
  }

  def getUserSharePermissions(workspaceId: String, userAccessLevel: WorkspaceAccessLevel, accessLevelToShareWith: WorkspaceAccessLevel): Future[Boolean] = {
    if (userAccessLevel < WorkspaceAccessLevels.Read) Future.successful(false)
    else if(userAccessLevel >= WorkspaceAccessLevels.Owner) Future.successful(true)
    else samDAO.userHasAction(SamResourceTypeNames.workspace, workspaceId, SamWorkspaceActions.sharePolicy(accessLevelToShareWith.toString.toLowerCase), userInfo)
  }

  def getUserCatalogPermissions(workspaceId: String): Future[Boolean] = {
    samDAO.userHasAction(SamResourceTypeNames.workspace, workspaceId, SamWorkspaceActions.catalog, userInfo)
  }

  /**
    * Sums up all the user's roles in a workspace to a single access level.
    *
    * USE FOR DISPLAY/USABILITY PURPOSES ONLY, NOT FOR REAL ACCESS DECISIONS
    * for real access decisions check actions in sam
    *
    * @param workspaceId
    * @return
    */
  def getMaximumAccessLevel(workspaceId: String): Future[WorkspaceAccessLevel] = {
    samDAO.listUserRolesForResource(SamResourceTypeNames.workspace, workspaceId, userInfo).map { roles =>
      roles.flatMap(role => WorkspaceAccessLevels.withRoleName(role.value)).fold(WorkspaceAccessLevels.NoAccess)(max)
    }
  }

  def getWorkspaceOwners(workspaceId: String): Future[Set[WorkbenchEmail]] = {
    samDAO.getPolicy(SamResourceTypeNames.workspace, workspaceId, SamWorkspacePolicyNames.owner, userInfo).map(_.memberEmails)
  }

  def getWorkspaceContext(workspaceName: WorkspaceName): Future[SlickWorkspaceContext] = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContext(workspaceName, dataAccess) { workspaceContext =>
        DBIO.successful(workspaceContext)
      }
    }
  }

  def getWorkspaceContextAndPermissions(workspaceName: WorkspaceName, requiredAction: SamResourceAction): Future[SlickWorkspaceContext] = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, requiredAction, dataAccess) { workspaceContext =>
        DBIO.successful(workspaceContext)
      }
    }
  }

  def deleteWorkspace(workspaceName: WorkspaceName): Future[PerRequestMessage] =  {
     getWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.delete) flatMap { ctx =>
       deleteWorkspace(workspaceName, ctx)
    }
  }

  private def deleteWorkspace(workspaceName: WorkspaceName, workspaceContext: SlickWorkspaceContext): Future[PerRequestMessage] = {
    //Attempt to abort any running workflows so they don't write any more to the bucket.
    //Notice that we're kicking off Futures to do the aborts concurrently, but we never collect their results!
    //This is because there's nothing we can do if Cromwell fails, so we might as well move on and let the
    //ExecutionContext run the futures whenever
    val deletionFuture: Future[Seq[WorkflowRecord]] = dataSource.inTransaction { dataAccess =>
      for {
        // Gather any active workflows with external ids
        workflowsToAbort <- dataAccess.workflowQuery.findActiveWorkflowsWithExternalIds(workspaceContext)

        //If a workflow is not done, automatically change its status to Aborted
        _ <- dataAccess.workflowQuery.findWorkflowsByWorkspace(workspaceContext).result.map { recs => recs.collect {
          case wf if !WorkflowStatuses.withName(wf.status).isDone =>
            dataAccess.workflowQuery.updateStatus(wf, WorkflowStatuses.Aborted) { status =>
              if (config.trackDetailedSubmissionMetrics) Option(workflowStatusCounter(workspaceSubmissionMetricBuilder(workspaceName, wf.submissionId))(status))
              else None
            }
        }}

        // Delete components of the workspace
        _ <- dataAccess.submissionQuery.deleteFromDb(workspaceContext.workspaceId)
        _ <- dataAccess.methodConfigurationQuery.deleteFromDb(workspaceContext.workspaceId)
        _ <- dataAccess.entityQuery.deleteFromDb(workspaceContext.workspaceId)

        // Delete the workspace
        _ <- dataAccess.workspaceQuery.delete(workspaceName)

        // Schedule bucket for deletion
        _ <- dataAccess.pendingBucketDeletionQuery.save(PendingBucketDeletionRecord(workspaceContext.workspace.bucketName))

      } yield {
        workflowsToAbort
      }
    }
    for {
      workflowsToAbort <- deletionFuture

      // Abort running workflows
      aborts = Future.traverse(workflowsToAbort) { wf => executionServiceCluster.abort(wf, userInfo) }

      // Delete resource in sam outside of DB transaction
      _ <- workspaceContext.workspace.workflowCollectionName.map( cn => samDAO.deleteResource(SamResourceTypeNames.workflowCollection, cn, userInfo) ).getOrElse(Future.successful(()))
      _ <- samDAO.deleteResource(SamResourceTypeNames.workspace, workspaceContext.workspaceId.toString, userInfo)
    } yield {
      aborts.onFailure {
        case t: Throwable => logger.info(s"failure aborting workflows while deleting workspace ${workspaceName}", t)
      }
      RequestComplete(StatusCodes.Accepted, s"Your Google bucket ${workspaceContext.workspace.bucketName} will be deleted within 24h.")
    }
  }

  def updateLibraryAttributes(workspaceName: WorkspaceName, operations: Seq[AttributeUpdateOperation]): Future[PerRequestMessage] = {
    withLibraryAttributeNamespaceCheck(operations.map(_.name)) {
      for {
        isCurator <- tryIsCurator(userInfo.userEmail)
        workspace <- getWorkspaceContext(workspaceName) flatMap { ctx =>
          withLibraryPermissions(ctx, operations, userInfo, isCurator) {
            dataSource.inTransaction ({ dataAccess =>
              updateWorkspace(operations, dataAccess)(ctx)
            }, TransactionIsolation.ReadCommitted) // read committed to avoid deadlocks on workspace attr scratch table
          }
        }
        authDomain <- loadResourceAuthDomain(SamResourceTypeNames.workspace, workspace.workspaceId, userInfo)
      } yield {
        RequestComplete(StatusCodes.OK, WorkspaceDetails(workspace, authDomain))
      }
    }
  }

  def updateWorkspace(workspaceName: WorkspaceName, operations: Seq[AttributeUpdateOperation]): Future[PerRequestMessage] = {
    withAttributeNamespaceCheck(operations.map(_.name)) {
      for {
        workspace <- dataSource.inTransaction({ dataAccess =>
          withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) {
            updateWorkspace(operations, dataAccess)
          }
        }, TransactionIsolation.ReadCommitted) // read committed to avoid deadlocks on workspace attr scratch table
        authDomain <- loadResourceAuthDomain(SamResourceTypeNames.workspace, workspace.workspaceId, userInfo)
      } yield {
        RequestComplete(StatusCodes.OK, WorkspaceDetails(workspace, authDomain))
      }
    }
  }

  private def updateWorkspace(operations: Seq[AttributeUpdateOperation], dataAccess: DataAccess)(workspaceContext: SlickWorkspaceContext): ReadWriteAction[Workspace] = {
    val workspace = workspaceContext.workspace
    Try {
      val updatedWorkspace = applyOperationsToWorkspace(workspace, operations)
      dataAccess.workspaceQuery.save(updatedWorkspace)
    } match {
      case Success(result) => result
      case Failure(e: AttributeUpdateOperationException) =>
        DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"Unable to update ${workspace.name}", ErrorReport(e))))
      case Failure(regrets) => DBIO.failed(regrets)
    }
  }

  def getTags(query: Option[String]): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      dataAccess.workspaceQuery.getTags(query).map { result =>
        RequestComplete(StatusCodes.OK, result)
      }
    }

  def listWorkspaces(params: WorkspaceFieldSpecs, parentSpan: Span): Future[PerRequestMessage] = {

    val s = startSpanWithParent("optionHandling", parentSpan)

    // validate the inbound parameters
    val options = Try(validateParams(params, WorkspaceFieldNames.workspaceListResponseFieldNames)) match {
      case Success(opts) => opts
      case Failure(ex) =>
        throw new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, ex))
    }

    // if user requested the entire attributes map, or any individual attributes, retrieve attributes.
    val attributeSpecs = WorkspaceAttributeSpecs(
      options.contains("workspace.attributes"),
      options.filter(_.startsWith("workspace.attributes."))
        .map(str => AttributeName.fromDelimitedName(str.replaceFirst("workspace.attributes.",""))).toList
    )

    // Can this be shared with get-workspace somehow?
    val optionsExist = options.nonEmpty
    val submissionStatsEnabled = options.contains("workspaceSubmissionStats")
    val attributesEnabled = attributeSpecs.all || attributeSpecs.attrsToSelect.nonEmpty

    s.setStatus(Status.OK)
    s.end()

    for {
      workspacePolicies <- traceWithParent("getPolicies", parentSpan)(_ => samDAO.getPoliciesForType(SamResourceTypeNames.workspace, userInfo))
      // filter out the policies that are not related to access levels, if a user has only those ignore the workspace
      accessLevelWorkspacePolicies = workspacePolicies.filter(p => WorkspaceAccessLevels.withPolicyName(p.accessPolicyName.value).nonEmpty)
      result <- dataSource.inTransaction({ dataAccess =>

        def workspaceSubmissionStatsFuture(): slick.ReadAction[Map[UUID, WorkspaceSubmissionStats]] = if (submissionStatsEnabled) {
          dataAccess.workspaceQuery.listSubmissionSummaryStats(accessLevelWorkspacePolicies.map(p => UUID.fromString(p.resourceId)).toSeq)
        } else {
          DBIO.from(Future(Map()))
        }

        val query = for {
          submissionSummaryStats <- traceDBIOWithParent("submissionStats", parentSpan)(_ => workspaceSubmissionStatsFuture())
          workspaces <- traceDBIOWithParent("listByIds", parentSpan)(_ => dataAccess.workspaceQuery.listByIds(accessLevelWorkspacePolicies.map(p => UUID.fromString(p.resourceId)).toSeq, Option(attributeSpecs)))
        } yield (submissionSummaryStats, workspaces)

        val results = traceDBIOWithParent("finalResults", parentSpan)(_ => query.map { case (submissionSummaryStats, workspaces) =>
          val policiesByWorkspaceId = accessLevelWorkspacePolicies.groupBy(_.resourceId).map { case (workspaceId, policies) =>
            workspaceId -> policies.reduce { (p1, p2) =>
              val betterAccessPolicyName = (WorkspaceAccessLevels.withPolicyName(p1.accessPolicyName.value), WorkspaceAccessLevels.withPolicyName(p2.accessPolicyName.value)) match {
                case (Some(p1Level), Some(p2Level)) if p1Level > p2Level => p1.accessPolicyName
                case (Some(_), Some(_)) => p2.accessPolicyName
                case _ => throw new RawlsException(s"unexpected state, both $p1 and $p2 should be related to access levels at this point")
              }
              SamResourceIdWithPolicyName(
                p1.resourceId,
                betterAccessPolicyName,
                p1.authDomainGroups ++ p2.authDomainGroups,
                p1.missingAuthDomainGroups ++ p2.missingAuthDomainGroups,
                p1.public || p2.public
              )
            }
          }
          workspaces.map { workspace =>
            val wsId = UUID.fromString(workspace.workspaceId)
            val workspacePolicy = policiesByWorkspaceId(workspace.workspaceId)
            val accessLevel = if (workspacePolicy.missingAuthDomainGroups.nonEmpty) WorkspaceAccessLevels.NoAccess else WorkspaceAccessLevels.withPolicyName(workspacePolicy.accessPolicyName.value).getOrElse(WorkspaceAccessLevels.NoAccess)
            // remove attributes if they were not requested
            val workspaceDetails = WorkspaceDetails.fromWorkspaceAndOptions(workspace, Option(workspacePolicy.authDomainGroups.map(groupName => ManagedGroupRef(RawlsGroupName(groupName.value)))), attributesEnabled)
            // remove submission stats if they were not requested
            val submissionStats: Option[WorkspaceSubmissionStats] = if (submissionStatsEnabled) {
              Option(submissionSummaryStats(wsId))
            } else {
              None
            }

            WorkspaceListResponse(accessLevel, workspaceDetails, submissionStats, workspacePolicy.public)
          }
        })

        results.map { responses =>
          if (!optionsExist) {
            RequestComplete(StatusCodes.OK, responses)
          } else {
            // perform json-filtering of payload
            RequestComplete(StatusCodes.OK, deepFilterJsValue(responses.toJson, options))
          }
        }
      }, TransactionIsolation.ReadCommitted)
    } yield result
  }

  private def getWorkspaceSubmissionStats(workspaceContext: SlickWorkspaceContext, dataAccess: DataAccess): ReadAction[WorkspaceSubmissionStats] = {
    // listSubmissionSummaryStats works against a sequence of workspaces; we call it just for this one workspace
    dataAccess.workspaceQuery
      .listSubmissionSummaryStats(Seq(workspaceContext.workspaceId))
      .map {p => p.get(workspaceContext.workspaceId).get}
  }

  def cloneWorkspace(sourceWorkspaceName: WorkspaceName, destWorkspaceRequest: WorkspaceRequest): Future[Workspace] = {
    destWorkspaceRequest.copyFilesWithPrefix.foreach(prefix => validateFileCopyPrefix(prefix))

    val (libraryAttributeNames, workspaceAttributeNames) = destWorkspaceRequest.attributes.keys.partition(name => name.namespace == AttributeName.libraryNamespace)
    withAttributeNamespaceCheck(workspaceAttributeNames) {
      withLibraryAttributeNamespaceCheck(libraryAttributeNames) {
        dataSource.inTransaction({ dataAccess =>
          withWorkspaceContextAndPermissions(sourceWorkspaceName, SamWorkspaceActions.read, dataAccess) { sourceWorkspaceContext =>
            DBIO.from(samDAO.getResourceAuthDomain(SamResourceTypeNames.workspace, sourceWorkspaceContext.workspace.workspaceId, userInfo)).flatMap { sourceAuthDomains =>
              withClonedAuthDomain(sourceAuthDomains.map(n => ManagedGroupRef(RawlsGroupName(n))).toSet, destWorkspaceRequest.authorizationDomain.getOrElse(Set.empty)) { newAuthDomain =>

                // add to or replace current attributes, on an individual basis
                val newAttrs = sourceWorkspaceContext.workspace.attributes ++ destWorkspaceRequest.attributes

                withNewWorkspaceContext(destWorkspaceRequest.copy(authorizationDomain = Option(newAuthDomain), attributes = newAttrs), dataAccess) { destWorkspaceContext =>
                  dataAccess.entityQuery.copyAllEntities(sourceWorkspaceContext, destWorkspaceContext) andThen
                    dataAccess.methodConfigurationQuery.listActive(sourceWorkspaceContext).flatMap { methodConfigShorts =>
                      val inserts = methodConfigShorts.map { methodConfigShort =>
                        dataAccess.methodConfigurationQuery.get(sourceWorkspaceContext, methodConfigShort.namespace, methodConfigShort.name).flatMap { methodConfig =>
                          dataAccess.methodConfigurationQuery.create(destWorkspaceContext, methodConfig.get)
                        }
                      }
                      DBIO.seq(inserts: _*)
                    } andThen {
                    DBIO.successful((sourceWorkspaceContext, destWorkspaceContext))
                  }
                }
              }
            }
          }
          // read committed to avoid deadlocks on workspace attr scratch table
        }, TransactionIsolation.ReadCommitted).map { case (sourceWorkspaceContext, destWorkspaceContext) =>
          //we will fire and forget this. a more involved, but robust, solution involves using the Google Storage Transfer APIs
          //in most of our use cases, these files should copy quickly enough for there to be no noticeable delay to the user
          //we also don't want to block returning a response on this call because it's already a slow endpoint
          destWorkspaceRequest.copyFilesWithPrefix.foreach { prefix =>
            copyBucketFiles(sourceWorkspaceContext, destWorkspaceContext, prefix)
          }

          destWorkspaceContext.workspace
        }
      }
    }
  }

  private def validateFileCopyPrefix(copyFilesWithPrefix: String): Unit = {
    if(copyFilesWithPrefix.isEmpty) throw new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, """You may not specify an empty string for `copyFilesWithPrefix`. Did you mean to specify "/" or leave the field out entirely?"""))
  }

  private def copyBucketFiles(sourceWorkspaceContext: SlickWorkspaceContext, destWorkspaceContext: SlickWorkspaceContext, copyFilesWithPrefix: String): Future[List[Option[StorageObject]]] = {
    gcsDAO.listObjectsWithPrefix(sourceWorkspaceContext.workspace.bucketName, copyFilesWithPrefix).flatMap { objectsToCopy =>
      Future.traverse(objectsToCopy) { objectToCopy =>  gcsDAO.copyFile(sourceWorkspaceContext.workspace.bucketName, objectToCopy.getName, destWorkspaceContext.workspace.bucketName, objectToCopy.getName) }
    }
  }

  private def withClonedAuthDomain[T](sourceWorkspaceADs: Set[ManagedGroupRef], destWorkspaceADs: Set[ManagedGroupRef])(op: (Set[ManagedGroupRef]) => ReadWriteAction[T]): ReadWriteAction[T] = {
    // if the source has an auth domain, the dest must also have that auth domain as a subset
    // otherwise, the caller may choose to add to the auth domain
    if(sourceWorkspaceADs.subsetOf(destWorkspaceADs)) op(sourceWorkspaceADs ++ destWorkspaceADs)
    else {
      val missingGroups = sourceWorkspaceADs -- destWorkspaceADs
      val errorMsg = s"Source workspace has an Authorization Domain containing the groups ${missingGroups.map(_.membersGroupName.value).mkString(", ")}, which are missing on the destination workspace"
      DBIO.failed(new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.UnprocessableEntity, errorMsg)))
    }
  }

  private def isUserPending(userEmail: String): Future[Boolean] = {
    samDAO.getUserIdInfo(userEmail, userInfo).map {
      case SamDAO.User(x) => x.googleSubjectId.isEmpty
      case SamDAO.NotUser => false
      case SamDAO.NotFound => true
    }
  }

  //API_CHANGE: project owners no longer returned (because it would just show a policy and not everyone can read the members of that policy)
  private def getACLInternal(workspaceName: WorkspaceName): Future[WorkspaceACL] = {

    def loadPolicy(policyName: SamResourcePolicyName, policyList: Set[SamPolicyWithNameAndEmail]): SamPolicyWithNameAndEmail = {
      policyList.find(_.policyName.value.equalsIgnoreCase(policyName.value)).getOrElse(throw new WorkbenchException(s"Could not load $policyName policy"))
    }

    val policyMembers = for {
      workspaceId <- loadWorkspaceId(workspaceName)
      currentACL <- samDAO.listPoliciesForResource(SamResourceTypeNames.workspace, workspaceId, userInfo)
    } yield {
      val ownerPolicyMembers = loadPolicy(SamWorkspacePolicyNames.owner, currentACL).policy.memberEmails
      val writerPolicyMembers = loadPolicy(SamWorkspacePolicyNames.writer, currentACL).policy.memberEmails
      val readerPolicyMembers = loadPolicy(SamWorkspacePolicyNames.reader, currentACL).policy.memberEmails
      val shareReaderPolicyMembers = loadPolicy(SamWorkspacePolicyNames.shareReader, currentACL).policy.memberEmails
      val shareWriterPolicyMembers = loadPolicy(SamWorkspacePolicyNames.shareWriter, currentACL).policy.memberEmails
      val computePolicyMembers = loadPolicy(SamWorkspacePolicyNames.canCompute, currentACL).policy.memberEmails
      //note: can-catalog is a policy on the side and is not a part of the core workspace ACL so we won't load it

      (ownerPolicyMembers, writerPolicyMembers, readerPolicyMembers, shareReaderPolicyMembers, shareWriterPolicyMembers, computePolicyMembers)
    }

    policyMembers.flatMap { case (ownerPolicyMembers, writerPolicyMembers, readerPolicyMembers, shareReaderPolicyMembers, shareWriterPolicyMembers, computePolicyMembers) =>
      val sharers = shareReaderPolicyMembers ++ shareWriterPolicyMembers

      for {
        ownersPending <- Future.traverse(ownerPolicyMembers) { email => isUserPending(email.value).map(pending => email -> pending) }
        writersPending <- Future.traverse(writerPolicyMembers) { email => isUserPending(email.value).map(pending => email -> pending) }
        readersPending <- Future.traverse(readerPolicyMembers) { email => isUserPending(email.value).map(pending => email -> pending) }
      } yield {
        val owners = ownerPolicyMembers.map(email => email.value -> AccessEntry(WorkspaceAccessLevels.Owner, ownersPending.toMap.getOrElse(email, true), true, true)) //API_CHANGE: pending owners used to show as false for canShare and canCompute. they now show true. this is more accurate anyway
        val writers = writerPolicyMembers.map(email => email.value -> AccessEntry(WorkspaceAccessLevels.Write, writersPending.toMap.getOrElse(email, true), sharers.contains(email), computePolicyMembers.contains(email)))
        val readers = readerPolicyMembers.map(email => email.value -> AccessEntry(WorkspaceAccessLevels.Read, readersPending.toMap.getOrElse(email, true), sharers.contains(email), computePolicyMembers.contains(email)))

        WorkspaceACL((owners ++ writers ++ readers).toMap)
      }
    }
  }

  def getACL(workspaceName: WorkspaceName): Future[PerRequestMessage] = {
    getACLInternal(workspaceName).map { acl => RequestComplete(StatusCodes.OK, acl)}
  }

  def getCatalog(workspaceName: WorkspaceName): Future[PerRequestMessage] = {
    loadWorkspaceId(workspaceName).flatMap { workspaceId =>
      samDAO.getPolicy(SamResourceTypeNames.workspace, workspaceId, SamWorkspacePolicyNames.canCatalog, userInfo).map { members => RequestComplete(StatusCodes.OK, members.memberEmails.map(email => WorkspaceCatalog(email.value, true)))}
    }
  }

  private def loadWorkspaceId(workspaceName: WorkspaceName): Future[String] = {
    dataSource.inTransaction { dataAccess => dataAccess.workspaceQuery.getWorkspaceId(workspaceName) }.map {
      case None => throw new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.NotFound, "unable to load workspace"))
      case Some(id) => id.toString
    }
  }

  def updateCatalog(workspaceName: WorkspaceName, input: Seq[WorkspaceCatalog]): Future[PerRequestMessage] = {
    for {
      workspaceId <- loadWorkspaceId(workspaceName)
      results <- Future.traverse(input) {
        case WorkspaceCatalog(email, true) =>
          toFutureTry(samDAO.addUserToPolicy(SamResourceTypeNames.workspace, workspaceId, SamWorkspacePolicyNames.canCatalog, email, userInfo)).
            map(_.map(_ => Either.right[String, WorkspaceCatalogResponse](WorkspaceCatalogResponse(email, true))).recover {
              case t: RawlsExceptionWithErrorReport if t.errorReport.statusCode.contains(StatusCodes.BadRequest) => Left(email)
            })

        case WorkspaceCatalog(email, false) =>
          toFutureTry(samDAO.removeUserFromPolicy(SamResourceTypeNames.workspace, workspaceId, SamWorkspacePolicyNames.canCatalog, email, userInfo)).
            map(_.map(_ => Either.right[String, WorkspaceCatalogResponse](WorkspaceCatalogResponse(email, false))).recover {
              case t: RawlsExceptionWithErrorReport if t.errorReport.statusCode.contains(StatusCodes.BadRequest) => Left(email)
            })
      }
    } yield {
      val failures = results.collect {
        case Failure(regrets) => ErrorReport(regrets)
      }
      if (failures.nonEmpty) {
        throw new RawlsExceptionWithErrorReport(ErrorReport("Error setting catalog permissions", failures))
      } else {
        RequestComplete(StatusCodes.OK, WorkspaceCatalogUpdateResponseList(results.collect { case Success(Right(wc)) => wc }, results.collect { case Success(Left(email)) => email }))
      }
    }
  }

  private def getWorkspacePolicies(workspaceName: WorkspaceName): Future[Set[SamPolicyWithNameAndEmail]] = {
    for {
      workspaceId <- loadWorkspaceId(workspaceName)
      policies <- samDAO.listPoliciesForResource(SamResourceTypeNames.workspace, workspaceId, userInfo)
    } yield policies
  }

  def collectMissingUsers(userEmails: Set[String]): Future[Set[String]] = {
    Future.traverse(userEmails) { email =>
      samDAO.getUserIdInfo(email, userInfo).map {
        case SamDAO.NotFound => Option(email)
        case _ => None
      }
    }.map(_.flatten)
  }

  /**
   * updates acls for a workspace
   * @param workspaceName
   * @param aclUpdates changes to make, if an entry already exists it will be changed to the level indicated in this
   *                   Set, use NoAccess to remove an entry, all other preexisting accesses remain unchanged
   * @return
   */
  def updateACL(workspaceName: WorkspaceName, aclUpdates: Set[WorkspaceACLUpdate], inviteUsersNotFound: Boolean): Future[PerRequestMessage] = {
    if (aclUpdates.map(_.email).size < aclUpdates.size) {
      throw new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, s"Only 1 entry per email allowed."))
    }

    /**
      * convert a set of policy names to the corresponding WorkspaceAclUpdate representation
      * @param userEmail
      * @param samWorkspacePolicyNames
      * @return
      */
    def policiesToAclUpdate(userEmail: String, samWorkspacePolicyNames: Set[SamResourcePolicyName]) = {
      val accessLevel = samWorkspacePolicyNames.flatMap(n => WorkspaceAccessLevels.withPolicyName(n.value)).fold(WorkspaceAccessLevels.NoAccess)(WorkspaceAccessLevels.max)
      val ownerLevel = samWorkspacePolicyNames.contains(SamWorkspacePolicyNames.projectOwner) || samWorkspacePolicyNames.contains(SamWorkspacePolicyNames.owner)
      val canShare = ownerLevel ||
        (samWorkspacePolicyNames.contains(SamWorkspacePolicyNames.reader) && samWorkspacePolicyNames.contains(SamWorkspacePolicyNames.shareReader)) ||
        (samWorkspacePolicyNames.contains(SamWorkspacePolicyNames.writer) && samWorkspacePolicyNames.contains(SamWorkspacePolicyNames.shareWriter))
      val canCompute = ownerLevel || samWorkspacePolicyNames.contains(SamWorkspacePolicyNames.canCompute)
      WorkspaceACLUpdate(userEmail, accessLevel, Option(canShare), Option(canCompute))
    }

    /**
      * convert a WorkspaceAclUpdate to the set of policy names that implement it
      * @param workspaceACLUpdate
      * @return
      */
    def aclUpdateToPolicies(workspaceACLUpdate: WorkspaceACLUpdate)= {
      val sharePolicy = workspaceACLUpdate match {
        case WorkspaceACLUpdate(_, WorkspaceAccessLevels.Read, Some(true), _) => SamWorkspacePolicyNames.shareReader.some
        case WorkspaceACLUpdate(_, WorkspaceAccessLevels.Write, Some(true), _) => SamWorkspacePolicyNames.shareWriter.some
        case _ => None
      }

      // canCompute is only applicable to write access, readers can't have it and owners have it implicitly
      val computePolicy = workspaceACLUpdate.canCompute match {
        case Some(false) => None
        case _ if workspaceACLUpdate.accessLevel == WorkspaceAccessLevels.Write => SamWorkspacePolicyNames.canCompute.some
        case _ => None
      }

      Set(workspaceACLUpdate.accessLevel.toPolicyName.map(SamResourcePolicyName), sharePolicy, computePolicy).flatten
    }

    def normalize(aclUpdates: Set[WorkspaceACLUpdate]) = {
      aclUpdates.map { update =>
        val ownerLevel = update.accessLevel >= WorkspaceAccessLevels.Owner
        val normalizedCanCompute = ownerLevel || update.canCompute.getOrElse(update.accessLevel == WorkspaceAccessLevels.Write)
        update.copy(canShare = Option(ownerLevel || update.canShare.getOrElse(false)), canCompute = Option(normalizedCanCompute)) }
    }

    collectMissingUsers(aclUpdates.map(_.email)).flatMap { userToInvite =>
      if (userToInvite.isEmpty || inviteUsersNotFound) {
        getWorkspacePolicies(workspaceName).flatMap { existingPolicies =>
          // the acl update code does not deal with the can catalog permission, there are separate functions for that.
          // exclude any existing can catalog policies so we don't inadvertently remove them
          val existingPoliciesExcludingCatalog = existingPolicies.filterNot(_.policyName == SamWorkspacePolicyNames.canCatalog)

          // convert all the existing policy memberships into WorkspaceAclUpdate objects
          val existingPoliciesWithMembers = existingPoliciesExcludingCatalog.flatMap(p => p.policy.memberEmails.map(email => email -> p.policyName))
          val existingAcls = existingPoliciesWithMembers.groupBy(_._1).map { case (email, policyNames) =>
            policiesToAclUpdate(email.value, policyNames.map(_._2))
          }.toSet

          // figure out which of the incoming aclUpdates are actually changes by removing all the existingAcls
          val aclChanges = normalize(aclUpdates) -- existingAcls
          validateAclChanges(aclChanges, existingAcls)

          // find users to remove from policies: existing policy members that are not in policies implied by aclChanges
          // note that access level No Access corresponds to 0 desired policies so all existing policies will be removed
          val policyRemovals = aclChanges.flatMap { aclChange =>
            val desiredPolicies = aclUpdateToPolicies(aclChange)
            existingPoliciesWithMembers.collect {
              case (email, policyName) if email.value.equalsIgnoreCase(aclChange.email) && !desiredPolicies.contains(policyName) => (policyName, aclChange.email)
            }
          }

          // find users to add to policies: users that are not existing policy members of policies implied by aclChanges
          val policyAdditions = aclChanges.flatMap { aclChange =>
            val desiredPolicies = aclUpdateToPolicies(aclChange)
            desiredPolicies.collect {
              case policyName if !existingPoliciesWithMembers.exists(x => x._1.value.equalsIgnoreCase(aclChange.email) && x._2 == policyName) => (policyName, aclChange.email)
            }
          }

          // now do all the work: invites, additions, removals, notifications
          for {
            maybeWorkspace <- dataSource.inTransaction { dataAccess => dataAccess.workspaceQuery.findByName(workspaceName) }
            workspace = maybeWorkspace.getOrElse(throw new RawlsException(s"workspace $workspaceName not found"))

            inviteNotifications <- Future.traverse(userToInvite) { invite =>
              samDAO.inviteUser(invite, userInfo).map { _ =>
                Notifications.WorkspaceInvitedNotification(RawlsUserEmail(invite), userInfo.userSubjectId, workspaceName, workspace.bucketName)
              }
            }

            // do additions before removals so users are not left unable to access the workspace in case of errors that
            // lead to incomplete application of these changes, remember: this is not transactional
            _ <- Future.traverse(policyAdditions) { case (policyName, email) =>
                samDAO.addUserToPolicy(SamResourceTypeNames.workspace, workspace.workspaceId, policyName, email, userInfo)
            }

            _ <- Future.traverse(policyRemovals) { case (policyName, email) =>
              samDAO.removeUserFromPolicy(SamResourceTypeNames.workspace, workspace.workspaceId, policyName, email, userInfo)
            }
            _ <- maybeShareProjectComputePolicy(policyAdditions, workspaceName)

          } yield {
            val (invites, updates) = aclChanges.partition(acl => userToInvite.contains(acl.email))
            sendACLUpdateNotifications(workspaceName, updates) //we can blindly fire off this future because we don't care about the results and it happens async anyway
            notificationDAO.fireAndForgetNotifications(inviteNotifications)
            RequestComplete(StatusCodes.OK, WorkspaceACLUpdateResponseList(updates, invites, Set.empty)) //API_CHANGE: no longer return invitesUpdated because you technically can't do that anymore...
          }
        }
      }
      else Future.successful(RequestComplete(StatusCodes.OK, WorkspaceACLUpdateResponseList(Set.empty, Set.empty, aclUpdates.filter(au => userToInvite.contains(au.email)))))
    }
  }

  private def validateAclChanges(aclChanges: Set[WorkspaceACLUpdate], existingAcls: Set[WorkspaceACLUpdate]) = {
    val emailsBeingChanged = aclChanges.map(_.email.toLowerCase)
    if (aclChanges.exists(_.accessLevel == WorkspaceAccessLevels.ProjectOwner) || existingAcls.exists(existingAcl => existingAcl.accessLevel == ProjectOwner && emailsBeingChanged.contains(existingAcl.email.toLowerCase))) {
      throw new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, "project owner permissions cannot be changed"))
    }
    if (aclChanges.exists(_.email.equalsIgnoreCase(userInfo.userEmail.value))) {
      throw new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, "you may not change your own permissions"))
    }
    if (aclChanges.exists {
      case WorkspaceACLUpdate(_, WorkspaceAccessLevels.Read, _, Some(true)) => true
      case _ => false
    }) {
      throw new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, "may not grant readers compute access"))
    }
  }

  // called from test harness
  private[workspace] def maybeShareProjectComputePolicy(policyAdditions: Set[(SamResourcePolicyName, String)], workspaceName: WorkspaceName): Future[Unit] = {
    val newWriterEmails = policyAdditions.collect {
      case (SamWorkspacePolicyNames.canCompute, email)  => email
    }
    Future.traverse(newWriterEmails) { email =>
      samDAO.addUserToPolicy(SamResourceTypeNames.billingProject, workspaceName.namespace, SamBillingProjectPolicyNames.canComputeUser, email, userInfo)
    }.map(_ => ())
  }

  private def sendACLUpdateNotifications(workspaceName: WorkspaceName, usersModified: Set[WorkspaceACLUpdate]): Unit = {
    Future.traverse(usersModified) { accessUpdate =>
      for {
        userIdInfo <- samDAO.getUserIdInfo(accessUpdate.email, userInfo)
      } yield {
        userIdInfo match {
          case SamDAO.User(UserIdInfo(_, _, Some(googleSubjectId))) =>
            if(accessUpdate.accessLevel == WorkspaceAccessLevels.NoAccess)
              notificationDAO.fireAndForgetNotification(Notifications.WorkspaceRemovedNotification(RawlsUserSubjectId(googleSubjectId), NoAccess.toString, workspaceName, userInfo.userSubjectId))
            else
              notificationDAO.fireAndForgetNotification(Notifications.WorkspaceAddedNotification(RawlsUserSubjectId(googleSubjectId), accessUpdate.accessLevel.toString, workspaceName, userInfo.userSubjectId))
          case _ =>
        }
      }
    }
  }

  def sendChangeNotifications(workspaceName: WorkspaceName): Future[PerRequestMessage] = {
    for {
      workspaceContext <- dataSource.inTransaction { dataAccess =>
        withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.own, dataAccess) { ctx => DBIO.successful(ctx) }
      }

      userIdInfos <- samDAO.listAllResourceMemberIds(SamResourceTypeNames.workspace, workspaceContext.workspace.workspaceId, userInfo)

      notificationMessages = userIdInfos.collect {
        case UserIdInfo(_, _, Some(userId)) => Notifications.WorkspaceChangedNotification(RawlsUserSubjectId(userId), workspaceName)
      }
    } yield {
      notificationDAO.fireAndForgetNotifications(notificationMessages)
      RequestComplete(StatusCodes.OK, notificationMessages.size.toString)
    }
  }

  def lockWorkspace(workspaceName: WorkspaceName): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContext(workspaceName, dataAccess) { workspaceContext =>
        requireAccessIgnoreLock(workspaceContext.workspace, SamWorkspaceActions.own) {
          dataAccess.submissionQuery.list(workspaceContext).flatMap { submissions =>
            if (!submissions.forall(_.status.isTerminated)) {
              DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Conflict, s"There are running submissions in workspace $workspaceName, so it cannot be locked.")))
            } else {
              dataAccess.workspaceQuery.lock(workspaceContext.workspace.toWorkspaceName).map(_ => RequestComplete(StatusCodes.NoContent))
            }
          }
        }
      }
    }

  def unlockWorkspace(workspaceName: WorkspaceName): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContext(workspaceName, dataAccess) { workspaceContext =>
        requireAccessIgnoreLock(workspaceContext.workspace, SamWorkspaceActions.own) {
          dataAccess.workspaceQuery.unlock(workspaceContext.workspace.toWorkspaceName).map(_ => RequestComplete(StatusCodes.NoContent))
        }
      }
    }

  def copyEntities(entityCopyDef: EntityCopyDefinition, uri: Uri, linkExistingEntities: Boolean): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(entityCopyDef.destinationWorkspace, SamWorkspaceActions.write, dataAccess) { destWorkspaceContext =>
        withWorkspaceContextAndPermissions(entityCopyDef.sourceWorkspace,SamWorkspaceActions.read, dataAccess) { sourceWorkspaceContext =>
          for {
            sourceAD <- DBIO.from(samDAO.getResourceAuthDomain(SamResourceTypeNames.workspace, sourceWorkspaceContext.workspace.workspaceId, userInfo))
            destAD <- DBIO.from(samDAO.getResourceAuthDomain(SamResourceTypeNames.workspace, destWorkspaceContext.workspace.workspaceId, userInfo))
            result <- authDomainCheck(sourceAD.toSet, destAD.toSet) flatMap { _ =>
              val entityNames = entityCopyDef.entityNames
              val entityType = entityCopyDef.entityType
              val copyResults = dataAccess.entityQuery.checkAndCopyEntities(sourceWorkspaceContext, destWorkspaceContext, entityType, entityNames, linkExistingEntities)
              copyResults.map { response =>
                if (response.hardConflicts.isEmpty && (response.softConflicts.isEmpty || linkExistingEntities)) RequestComplete(StatusCodes.Created, response)
                else RequestComplete(StatusCodes.Conflict, response)
              }
            }
          } yield result
        }
      }
    }

  // can't use withClonedAuthDomain because the Auth Domain -> no Auth Domain logic is different
  private def authDomainCheck(sourceWorkspaceADs: Set[String], destWorkspaceADs: Set[String]): ReadWriteAction[Boolean] = {
    // if the source has any auth domains, the dest must also *at least* have those auth domains
    if(sourceWorkspaceADs.subsetOf(destWorkspaceADs)) DBIO.successful(true)
    else {
      val missingGroups = sourceWorkspaceADs -- destWorkspaceADs
      val errorMsg = s"Source workspace has an Authorization Domain containing the groups ${missingGroups.mkString(", ")}, which are missing on the destination workspace"
      DBIO.failed(new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.UnprocessableEntity, errorMsg)))
    }
  }

  def createEntity(workspaceName: WorkspaceName, entity: Entity): Future[Entity] =
    withAttributeNamespaceCheck(entity) {
      dataSource.inTransaction { dataAccess =>
        withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
          dataAccess.entityQuery.get(workspaceContext, entity.entityType, entity.name) flatMap {
            case Some(_) => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Conflict, s"${entity.entityType} ${entity.name} already exists in ${workspaceName}")))
            case None => dataAccess.entityQuery.save(workspaceContext, entity)
          }
        }
      }
    }

  def batchUpdateEntities(workspaceName: WorkspaceName, entityUpdates: Seq[EntityUpdateDefinition], upsert: Boolean = false): Future[PerRequestMessage] = {
    val namesToCheck = for {
      update <- entityUpdates
      operation <- update.operations
    } yield operation.name

    withAttributeNamespaceCheck(namesToCheck) {
      dataSource.inTransaction { dataAccess =>
        withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
          val updateTrialsAction = dataAccess.entityQuery.getActiveEntities(workspaceContext, entityUpdates.map(eu => AttributeEntityReference(eu.entityType, eu.name))) map { entities =>
            val entitiesByName = entities.map(e => (e.entityType, e.name) -> e).toMap
            entityUpdates.map { entityUpdate =>
              entityUpdate -> (entitiesByName.get((entityUpdate.entityType, entityUpdate.name)) match {
                case Some(e) =>
                  Try(applyOperationsToEntity(e, entityUpdate.operations))
                case None =>
                  if (upsert) {
                    Try(applyOperationsToEntity(Entity(entityUpdate.name, entityUpdate.entityType, Map.empty), entityUpdate.operations))
                  } else {
                    Failure(new RuntimeException("Entity does not exist"))
                  }
              })
            }
          }

          val saveAction = updateTrialsAction flatMap { updateTrials =>
            val errorReports = updateTrials.collect { case (entityUpdate, Failure(regrets)) =>
              ErrorReport(s"Could not update ${entityUpdate.entityType} ${entityUpdate.name}", ErrorReport(regrets))
            }
            if (!errorReports.isEmpty) {
              DBIO.failed(new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, "Some entities could not be updated.", errorReports)))
            } else {
              val t = updateTrials.collect { case (entityUpdate, Success(entity)) => entity }

              dataAccess.entityQuery.save(workspaceContext, t)
            }
          }

          saveAction.map(_ => RequestComplete(StatusCodes.NoContent))
        }
      }
    }
  }

  def entityTypeMetadata(workspaceName: WorkspaceName): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        dataAccess.entityQuery.getEntityTypeMetadata(workspaceContext).map(r => RequestComplete(StatusCodes.OK, r))
      }
    }

  def listEntities(workspaceName: WorkspaceName, entityType: String): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        dataAccess.entityQuery.listActiveEntitiesOfType(workspaceContext, entityType).map(r => RequestComplete(StatusCodes.OK, r.toSeq))
      }
    }

  def queryEntities(workspaceName: WorkspaceName, entityType: String, query: EntityQuery): Future[PerRequestMessage] = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        dataAccess.entityQuery.loadEntityPage(workspaceContext, entityType, query) map { case (unfilteredCount, filteredCount, entities) =>
          createEntityQueryResponse(query, unfilteredCount, filteredCount, entities.toSeq).get
        }
      }
    }
  }

  def createEntityQueryResponse(query: EntityQuery, unfilteredCount: Int, filteredCount: Int, page: Seq[Entity]): Try[RequestComplete[(StatusCodes.Success, EntityQueryResponse)]] = {
    val pageCount = Math.ceil(filteredCount.toFloat / query.pageSize).toInt
    if (filteredCount > 0 && query.page > pageCount) {
      Failure(new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, s"requested page ${query.page} is greater than the number of pages $pageCount")))

    } else {
      val response = EntityQueryResponse(query, EntityQueryResultMetadata(unfilteredCount, filteredCount, pageCount), page)

      Success(RequestComplete(StatusCodes.OK, response))
    }
  }

  def getEntity(workspaceName: WorkspaceName, entityType: String, entityName: String): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withEntity(workspaceContext, entityType, entityName, dataAccess) { entity =>
          DBIO.successful(PerRequest.RequestComplete(StatusCodes.OK, entity))
        }
      }
    }

  def updateEntity(workspaceName: WorkspaceName, entityType: String, entityName: String, operations: Seq[AttributeUpdateOperation]): Future[PerRequestMessage] =
    withAttributeNamespaceCheck(operations.map(_.name)) {
      dataSource.inTransaction { dataAccess =>
        withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
          withEntity(workspaceContext, entityType, entityName, dataAccess) { entity =>
            val updateAction = Try {
              val updatedEntity = applyOperationsToEntity(entity, operations)
              dataAccess.entityQuery.save(workspaceContext, updatedEntity)
            } match {
              case Success(result) => result
              case Failure(e: AttributeUpdateOperationException) =>
                DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"Unable to update entity ${entityType}/${entityName} in ${workspaceName}", ErrorReport(e))))
              case Failure(regrets) => DBIO.failed(regrets)
            }
            updateAction.map(RequestComplete(StatusCodes.OK, _))
          }
        }
      }
    }

  def deleteEntities(workspaceName: WorkspaceName, entRefs: Seq[AttributeEntityReference]): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
        withAllEntities(workspaceContext, dataAccess, entRefs) { entities =>
          dataAccess.entityQuery.getAllReferringEntities(workspaceContext, entRefs.toSet) flatMap { referringEntities =>
            if (referringEntities != entRefs.toSet)
              DBIO.successful(RequestComplete(StatusCodes.Conflict, referringEntities))
            else {
              dataAccess.entityQuery.hide(workspaceContext, entRefs).map(_ => RequestComplete(StatusCodes.NoContent))
            }
          }
        }
      }
    }

  def renameEntity(workspaceName: WorkspaceName, entityType: String, entityName: String, newName: String): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
        withEntity(workspaceContext, entityType, entityName, dataAccess) { entity =>
          dataAccess.entityQuery.get(workspaceContext, entity.entityType, newName) flatMap {
            case None => dataAccess.entityQuery.rename(workspaceContext, entity.entityType, entity.name, newName)
            case Some(_) => throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Conflict, s"Destination ${entity.entityType} ${newName} already exists"))
          } map(_ => RequestComplete(StatusCodes.NoContent))
        }
      }
    }

  def evaluateExpression(workspaceName: WorkspaceName, entityType: String, entityName: String, expression: String): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withSingleEntityRec(entityType, entityName, workspaceContext, dataAccess) { entities =>
          ExpressionEvaluator.withNewExpressionEvaluator(dataAccess, Some(entities)) { evaluator =>
            evaluator.evalFinalAttribute(workspaceContext, expression).asTry map { tryValuesByEntity => tryValuesByEntity match {
              //parsing failure
              case Failure(regret) => throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, regret))
              case Success(valuesByEntity) =>
                if (valuesByEntity.size != 1) {
                  //wrong number of entities?!
                  throw new RawlsException(s"Expression parsing should have returned a single entity for ${entityType}/$entityName $expression, but returned ${valuesByEntity.size} entities instead")
                } else {
                  assert(valuesByEntity.head._1 == entityName)
                  valuesByEntity.head match {
                    case (_, Success(result)) => RequestComplete(StatusCodes.OK, result.toSeq)
                    case (_, Failure(regret)) =>
                      throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, "Unable to evaluate expression '${expression}' on ${entityType}/${entityName} in ${workspaceName}", ErrorReport(regret)))
                  }
                }
              }
            }
          }
        }
      }
    }

  /**
   * Applies the sequence of operations in order to the entity.
   *
   * @param entity to update
   * @param operations sequence of operations
   * @throws AttributeNotFoundException when removing from a list attribute that does not exist
   * @throws AttributeUpdateOperationException when adding or removing from an attribute that is not a list
   * @return the updated entity
   */
  def applyOperationsToEntity(entity: Entity, operations: Seq[AttributeUpdateOperation]): Entity = {
    entity.copy(attributes = applyAttributeUpdateOperations(entity, operations))
  }

  /**
   * Applies the sequence of operations in order to the workspace.
   *
   * @param workspace to update
   * @param operations sequence of operations
   * @throws AttributeNotFoundException when removing from a list attribute that does not exist
   * @throws AttributeUpdateOperationException when adding or removing from an attribute that is not a list
   * @return the updated entity
   */
  def applyOperationsToWorkspace(workspace: Workspace, operations: Seq[AttributeUpdateOperation]): Workspace = {
    workspace.copy(attributes = applyAttributeUpdateOperations(workspace, operations))
  }

  private def applyAttributeUpdateOperations(attributable: Attributable, operations: Seq[AttributeUpdateOperation]): AttributeMap = {
    operations.foldLeft(attributable.attributes) { (startingAttributes, operation) =>

      operation match {
        case AddUpdateAttribute(attributeName, attribute) => startingAttributes + (attributeName -> attribute)

        case RemoveAttribute(attributeName) => startingAttributes - attributeName

        case CreateAttributeEntityReferenceList(attributeName) =>
          if( startingAttributes.contains(attributeName) ) { //non-destructive
            startingAttributes
          } else {
            startingAttributes + (attributeName -> AttributeEntityReferenceEmptyList)
          }

        case CreateAttributeValueList(attributeName) =>
          if( startingAttributes.contains(attributeName) ) { //non-destructive
            startingAttributes
          } else {
            startingAttributes + (attributeName -> AttributeValueEmptyList)
          }

        case AddListMember(attributeListName, newMember) =>
          startingAttributes.get(attributeListName) match {
            case Some(AttributeValueEmptyList) =>
              newMember match {
                case AttributeNull =>
                  startingAttributes
                case newMember: AttributeValue =>
                  startingAttributes + (attributeListName -> AttributeValueList(Seq(newMember)))
                case newMember: AttributeEntityReference =>
                  throw new AttributeUpdateOperationException("Cannot add non-value to list of values.")
                case _ => throw new AttributeUpdateOperationException("Cannot create list with that type.")
              }

            case Some(AttributeEntityReferenceEmptyList) =>
              newMember match {
                case AttributeNull =>
                  startingAttributes
                case newMember: AttributeEntityReference =>
                  startingAttributes + (attributeListName -> AttributeEntityReferenceList(Seq(newMember)))
                case newMember: AttributeValue =>
                  throw new AttributeUpdateOperationException("Cannot add non-reference to list of references.")
                case _ => throw new AttributeUpdateOperationException("Cannot create list with that type.")
              }

            case Some(l: AttributeValueList) =>
              newMember match {
                case AttributeNull =>
                  startingAttributes
                case newMember: AttributeValue =>
                  startingAttributes + (attributeListName -> AttributeValueList(l.list :+ newMember))
                case _ => throw new AttributeUpdateOperationException("Cannot add non-value to list of values.")
              }

            case Some(l: AttributeEntityReferenceList) =>
              newMember match {
                case AttributeNull =>
                  startingAttributes
                case newMember: AttributeEntityReference =>
                  startingAttributes + (attributeListName -> AttributeEntityReferenceList(l.list :+ newMember))
                case _ => throw new AttributeUpdateOperationException("Cannot add non-reference to list of references.")
              }

            case None =>
              newMember match {
                case AttributeNull =>
                  throw new AttributeUpdateOperationException("Cannot use AttributeNull to create empty list. Use CreateEmpty[Ref|Val]List instead.")
                case newMember: AttributeValue =>
                  startingAttributes + (attributeListName -> AttributeValueList(Seq(newMember)))
                case newMember: AttributeEntityReference =>
                  startingAttributes + (attributeListName -> AttributeEntityReferenceList(Seq(newMember)))
                case _ => throw new AttributeUpdateOperationException("Cannot create list with that type.")
              }

            case Some(_) => throw new AttributeUpdateOperationException(s"$attributeListName of ${attributable.briefName} is not a list")
          }

        case RemoveListMember(attributeListName, removeMember) =>
          startingAttributes.get(attributeListName) match {
            case Some(l: AttributeValueList) =>
              startingAttributes + (attributeListName -> AttributeValueList(l.list.filterNot(_ == removeMember)))
            case Some(l: AttributeEntityReferenceList) =>
              startingAttributes + (attributeListName -> AttributeEntityReferenceList(l.list.filterNot(_ == removeMember)))
            case None => throw new AttributeNotFoundException(s"$attributeListName of ${attributable.briefName} does not exist")
            case Some(_) => throw new AttributeUpdateOperationException(s"$attributeListName of ${attributable.briefName} is not a list")
          }
      }
    }
  }

  //validates the expressions in the method configuration, taking into account optional inputs
  private def validateMethodConfiguration(methodConfiguration: MethodConfiguration, dataAccess: DataAccess): ReadWriteAction[ValidatedMethodConfiguration] = {
    withMethodInputs(methodConfiguration, userInfo) { (_, gatherInputsResult) =>
      val vmc = ExpressionValidator.validateAndParseMCExpressions(methodConfiguration, gatherInputsResult, allowRootEntity = methodConfiguration.rootEntityType.isDefined, dataAccess)
      DBIO.successful(vmc)
    }
  }

  def createMCAndValidateExpressions(workspaceContext: SlickWorkspaceContext, methodConfiguration: MethodConfiguration, dataAccess: DataAccess): ReadWriteAction[ValidatedMethodConfiguration] = {
    dataAccess.methodConfigurationQuery.create(workspaceContext, methodConfiguration) flatMap { _ =>
      validateMethodConfiguration(methodConfiguration, dataAccess)
    }
  }

  def updateMCAndValidateExpressions(workspaceContext: SlickWorkspaceContext, methodConfigurationNamespace: String, methodConfigurationName: String, methodConfiguration: MethodConfiguration, dataAccess: DataAccess): ReadWriteAction[ValidatedMethodConfiguration] = {
    dataAccess.methodConfigurationQuery.update(workspaceContext, methodConfigurationNamespace, methodConfigurationName, methodConfiguration) flatMap { _ =>
      validateMethodConfiguration(methodConfiguration, dataAccess)
    }
  }

  def getAndValidateMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String): Future[PerRequestMessage] = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withMethodConfig(workspaceContext, methodConfigurationNamespace, methodConfigurationName, dataAccess) { methodConfig =>
          validateMethodConfiguration(methodConfig, dataAccess) map { vmc =>
            PerRequest.RequestComplete(StatusCodes.OK, vmc)
          }
        }
      }
    }
  }

  def createMethodConfiguration(workspaceName: WorkspaceName, methodConfiguration: MethodConfiguration): Future[ValidatedMethodConfiguration] = {
    withAttributeNamespaceCheck(methodConfiguration) {
      dataSource.inTransaction { dataAccess =>
        withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
          dataAccess.methodConfigurationQuery.get(workspaceContext, methodConfiguration.namespace, methodConfiguration.name) flatMap {
            case Some(_) => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Conflict, s"${methodConfiguration.name} already exists in ${workspaceName}")))
            case None => createMCAndValidateExpressions(workspaceContext, methodConfiguration, dataAccess)
          }
        }
      }
    }
  }

  def deleteMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
        withMethodConfig(workspaceContext, methodConfigurationNamespace, methodConfigurationName, dataAccess) { methodConfig =>
          dataAccess.methodConfigurationQuery.delete(workspaceContext, methodConfigurationNamespace, methodConfigurationName).map(_ => RequestComplete(StatusCodes.NoContent))
        }
      }
    }

  def renameMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String, newName: MethodConfigurationName): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
        //It's terrible that we pass unnecessary junk that we don't read in the payload, but a big refactor of the API is going to have to wait until Some Other Time.
        if(newName.workspaceName != workspaceName) {
          DBIO.failed(new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.BadRequest, "Workspace name and namespace in payload must match those in the URI")))
        } else {
          withMethodConfig(workspaceContext, methodConfigurationNamespace, methodConfigurationName, dataAccess) { methodConfiguration =>
            //If a different MC exists at the target location, return 409. But it's okay to want to overwrite your own MC.
            dataAccess.methodConfigurationQuery.get(workspaceContext, newName.namespace, newName.name) flatMap {
              case Some(_) if methodConfigurationNamespace != newName.namespace || methodConfigurationName != newName.name =>
                DBIO.failed(new RawlsExceptionWithErrorReport(errorReport =
                  ErrorReport(StatusCodes.Conflict, s"There is already a method configuration at ${methodConfiguration.namespace}/${methodConfiguration.name} in ${workspaceName}.")))
              case Some(_) => DBIO.successful(()) //renaming self to self: no-op
              case None =>
                dataAccess.methodConfigurationQuery.update(workspaceContext, methodConfigurationNamespace, methodConfigurationName, methodConfiguration.copy(name = newName.name, namespace = newName.namespace))
            } map (_ => RequestComplete(StatusCodes.NoContent))
          }
        }
      }
    }

  //Overwrite the method configuration at methodConfiguration[namespace|name] with the new method configuration.
  def overwriteMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String, methodConfiguration: MethodConfiguration): Future[ValidatedMethodConfiguration] = {
    withAttributeNamespaceCheck(methodConfiguration) {
      // create transaction
      dataSource.inTransaction { dataAccess =>
        // check permissions
        withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
          if(methodConfiguration.namespace != methodConfigurationNamespace || methodConfiguration.name != methodConfigurationName) {
            DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest,
              s"The method configuration name and namespace in the URI should match the method configuration name and namespace in the request body. If you want to move this method configuration, use POST.")))
          } else {
            createMCAndValidateExpressions(workspaceContext, methodConfiguration, dataAccess)
          }
        }
      }
    }
  }

  //Move the method configuration at methodConfiguration[namespace|name] to the location specified in methodConfiguration, _and_ update it.
  //It's like a rename and upsert all rolled into one.
  def updateMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String, methodConfiguration: MethodConfiguration): Future[PerRequestMessage] = {
    withAttributeNamespaceCheck(methodConfiguration) {
     // create transaction
      dataSource.inTransaction { dataAccess =>
       // check permissions
        withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
          withMethodConfig(workspaceContext, methodConfigurationNamespace, methodConfigurationName, dataAccess) { _ =>
              dataAccess.methodConfigurationQuery.get(workspaceContext, methodConfiguration.namespace, methodConfiguration.name) flatMap {
                //If a different MC exists at the target location, return 409. But it's okay to want to overwrite your own MC.
                case Some(_) if methodConfigurationNamespace != methodConfiguration.namespace || methodConfigurationName != methodConfiguration.name =>
                  DBIO.failed(new RawlsExceptionWithErrorReport(errorReport =
                    ErrorReport(StatusCodes.Conflict, s"There is already a method configuration at ${methodConfiguration.namespace}/${methodConfiguration.name} in ${workspaceName}.")))
                case _ =>
                  updateMCAndValidateExpressions(workspaceContext, methodConfigurationNamespace, methodConfigurationName, methodConfiguration, dataAccess)
              } map (RequestComplete(StatusCodes.OK, _))
          }
        }
      }
    }
  }

  def getMethodConfiguration(workspaceName: WorkspaceName, methodConfigurationNamespace: String, methodConfigurationName: String): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withMethodConfig(workspaceContext, methodConfigurationNamespace, methodConfigurationName, dataAccess) { methodConfig =>
          DBIO.successful(PerRequest.RequestComplete(StatusCodes.OK, methodConfig))
        }
      }
    }

  def copyMethodConfiguration(mcnp: MethodConfigurationNamePair): Future[ValidatedMethodConfiguration] = {
    // split into two transactions because we need to call out to Google after retrieving the source MC

    val transaction1Result = dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(mcnp.destination.workspaceName, SamWorkspaceActions.write, dataAccess) { destContext =>
        withWorkspaceContextAndPermissions(mcnp.source.workspaceName, SamWorkspaceActions.read, dataAccess) { sourceContext =>
          dataAccess.methodConfigurationQuery.get(sourceContext, mcnp.source.namespace, mcnp.source.name) flatMap {
            case None =>
              val err = ErrorReport(StatusCodes.NotFound, s"There is no method configuration named ${mcnp.source.namespace}/${mcnp.source.name} in ${mcnp.source.workspaceName}.")
              DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = err))
            case Some(methodConfig) => DBIO.successful((methodConfig, destContext))
          }
        }
      }
    }

    transaction1Result flatMap { case (methodConfig, destContext) =>
      withAttributeNamespaceCheck(methodConfig) {
        dataSource.inTransaction { dataAccess =>
          saveCopiedMethodConfiguration(methodConfig, mcnp.destination, destContext, dataAccess)
        }
      }
    }
  }

  def copyMethodConfigurationFromMethodRepo(methodRepoQuery: MethodRepoConfigurationImport): Future[ValidatedMethodConfiguration] =
    methodRepoDAO.getMethodConfig(methodRepoQuery.methodRepoNamespace, methodRepoQuery.methodRepoName, methodRepoQuery.methodRepoSnapshotId, userInfo) flatMap {
      case None =>
        val name = s"${methodRepoQuery.methodRepoNamespace}/${methodRepoQuery.methodRepoName}/${methodRepoQuery.methodRepoSnapshotId}"
        val err = ErrorReport(StatusCodes.NotFound, s"There is no method configuration named $name in the repository.")
        Future.failed(new RawlsExceptionWithErrorReport(errorReport = err))
      case Some(agoraEntity) => Future.fromTry(parseAgoraEntity(agoraEntity)) flatMap { targetMethodConfig =>
        withAttributeNamespaceCheck(targetMethodConfig) {
          dataSource.inTransaction { dataAccess =>
            withWorkspaceContextAndPermissions(methodRepoQuery.destination.workspaceName, SamWorkspaceActions.write, dataAccess) { destContext =>
              saveCopiedMethodConfiguration(targetMethodConfig, methodRepoQuery.destination, destContext, dataAccess)
            }
          }
        }
      }
    }

  private def parseAgoraEntity(agoraEntity: AgoraEntity): Try[MethodConfiguration] = {
    val parsed = Try {
      agoraEntity.payload.map(JsonParser(_).convertTo[AgoraMethodConfiguration])
    } recoverWith {
      case e: Exception =>
        Failure(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.UnprocessableEntity, "Error parsing Method Repo response message.", ErrorReport(e))))
    }

    parsed flatMap {
      case Some(agoraMC) => Success(convertToMethodConfiguration(agoraMC))
      case None => Failure(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.UnprocessableEntity, "Method Repo missing configuration payload")))
    }
  }

  private def convertToMethodConfiguration(agoraMethodConfig: AgoraMethodConfiguration): MethodConfiguration = {
    MethodConfiguration(agoraMethodConfig.namespace, agoraMethodConfig.name, Some(agoraMethodConfig.rootEntityType), Some(Map.empty[String, AttributeString]), agoraMethodConfig.inputs, agoraMethodConfig.outputs, agoraMethodConfig.methodRepoMethod)
  }

  def copyMethodConfigurationToMethodRepo(methodRepoQuery: MethodRepoConfigurationExport): Future[PerRequestMessage] = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(methodRepoQuery.source.workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withMethodConfig(workspaceContext, methodRepoQuery.source.namespace, methodRepoQuery.source.name, dataAccess) { methodConfig =>

          DBIO.from(methodRepoDAO.postMethodConfig(
            methodRepoQuery.methodRepoNamespace,
            methodRepoQuery.methodRepoName,
            methodConfig.copy(namespace = methodRepoQuery.methodRepoNamespace, name = methodRepoQuery.methodRepoName),
            userInfo)) map { RequestComplete(StatusCodes.OK, _) }
        }
      }
    }
  }

  private def saveCopiedMethodConfiguration(methodConfig: MethodConfiguration, dest: MethodConfigurationName, destContext: SlickWorkspaceContext, dataAccess: DataAccess) = {
    val target = methodConfig.copy(name = dest.name, namespace = dest.namespace)

    dataAccess.methodConfigurationQuery.get(destContext, dest.namespace, dest.name).flatMap {
      case Some(existingMethodConfig) => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Conflict, s"A method configuration named ${dest.namespace}/${dest.name} already exists in ${dest.workspaceName}")))
      case None => createMCAndValidateExpressions(destContext, target, dataAccess)
    }
  }

  def listAgoraMethodConfigurations(workspaceName: WorkspaceName): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        dataAccess.methodConfigurationQuery.listActive(workspaceContext).map { r =>
          RequestComplete(StatusCodes.OK, r.toList.filter(_.methodRepoMethod.repo == Agora))
        }
      }
    }

  def listMethodConfigurations(workspaceName: WorkspaceName): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        dataAccess.methodConfigurationQuery.listActive(workspaceContext).map { r =>
          RequestComplete(StatusCodes.OK, r.toList)
        }
      }
    }

  def createMethodConfigurationTemplate(methodRepoMethod: MethodRepoMethod ): Future[PerRequestMessage] = {
    dataSource.inTransaction { dataAccess =>
      withMethod(methodRepoMethod, userInfo) { method =>
        withWdl(method) { wdl => methodConfigResolver.toMethodConfiguration(userInfo, wdl, methodRepoMethod) match {
          case Failure(exception) => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, exception)))
          case Success(methodConfig) => DBIO.successful(RequestComplete(StatusCodes.OK, methodConfig))
        }}
      }
    }
  }

  def getMethodInputsOutputs(userInfo: UserInfo, methodRepoMethod: MethodRepoMethod ): Future[PerRequestMessage] = {
    dataSource.inTransaction { dataAccess =>
      withMethod(methodRepoMethod, userInfo) { method =>
        withWdl(method) { wdl => methodConfigResolver.getMethodInputsOutputs(userInfo, wdl) match {
          case Failure(exception) => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, exception)))
          case Success(inputsOutputs) => DBIO.successful(RequestComplete(StatusCodes.OK, inputsOutputs))
        }}
      }
    }
  }

  def listSubmissions(workspaceName: WorkspaceName): Future[PerRequestMessage] = {
    val costlessSubmissionsFuture = dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        dataAccess.submissionQuery.listWithSubmitter(workspaceContext)
      }
    }

    // TODO David An 2018-05-30: temporarily disabling cost calculations for submission list due to potential performance hit
    // val costMapFuture = costlessSubmissionsFuture flatMap { submissions =>
    //   submissionCostService.getWorkflowCosts(submissions.flatMap(_.workflowIds).flatten, workspaceName.namespace)
    // }
    val costMapFuture = Future.successful(Map.empty[String,Float])

    toFutureTry(costMapFuture) flatMap { costMapTry =>
      val costMap: Map[String,Float] = costMapTry match {
        case Failure(ex) =>
          logger.error("Unable to get cost data from BigQuery", ex)
          Map()
        case Success(costs) => costs
      }

      costlessSubmissionsFuture map { costlessSubmissions =>
        val costedSubmissions = costlessSubmissions map { costlessSubmission =>
          // TODO David An 2018-05-30: temporarily disabling cost calculations for submission list due to potential performance hit
          // val summedCost = costlessSubmission.workflowIds.map { workflowIds => workflowIds.flatMap(costMap.get).sum }
          val summedCost = None
          // Clearing workflowIds is a quick fix to prevent SubmissionListResponse from having too much data. Will address in the near future.
          costlessSubmission.copy(cost = summedCost, workflowIds = None)
        }
        RequestComplete(StatusCodes.OK, costedSubmissions)
      }
    }
  }

  def countSubmissions(workspaceName: WorkspaceName): Future[PerRequestMessage] =
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        dataAccess.submissionQuery.countByStatus(workspaceContext).map(RequestComplete(StatusCodes.OK, _))
      }
    }

  def createSubmission(workspaceName: WorkspaceName, submissionRequest: SubmissionRequest): Future[PerRequestMessage] = {
    withSubmissionParameters(workspaceName, submissionRequest) {
      (dataAccess: DataAccess, workspaceContext: SlickWorkspaceContext, wdl: String, header: SubmissionValidationHeader, successes: Seq[SubmissionValidationEntityInputs], failures: Seq[SubmissionValidationEntityInputs], workflowFailureMode: Option[WorkflowFailureMode]) =>
        requireComputePermission(workspaceContext.workspace) {
          val submissionId: UUID = UUID.randomUUID()
          val submissionEntityOpt = if(header.entityType.isEmpty) { None } else { Some(AttributeEntityReference(entityType = submissionRequest.entityType.get, entityName = submissionRequest.entityName.get)) }

          val workflows = successes map { entityInputs =>
            val workflowEntityOpt = header.entityType.map(_ => AttributeEntityReference(entityType = header.entityType.get, entityName = entityInputs.entityName))
            Workflow(workflowId = None,
              status = WorkflowStatuses.Queued,
              statusLastChangedDate = DateTime.now,
              workflowEntity = workflowEntityOpt,
              inputResolutions = entityInputs.inputResolutions.toSeq
            )
          }

          val workflowFailures = failures map { entityInputs =>
            val workflowEntityOpt = header.entityType.map(_ => AttributeEntityReference(entityType = header.entityType.get, entityName = entityInputs.entityName))
            Workflow(workflowId = None,
              status = WorkflowStatuses.Failed,
              statusLastChangedDate = DateTime.now,
              workflowEntity = workflowEntityOpt,
              inputResolutions = entityInputs.inputResolutions.toSeq,
              messages = (for (entityValue <- entityInputs.inputResolutions if entityValue.error.isDefined) yield AttributeString(entityValue.inputName + " - " + entityValue.error.get)).toSeq
            )
          }

          val submission = Submission(submissionId = submissionId.toString,
            submissionDate = DateTime.now(),
            submitter = WorkbenchEmail(userInfo.userEmail.value),
            methodConfigurationNamespace = submissionRequest.methodConfigurationNamespace,
            methodConfigurationName = submissionRequest.methodConfigurationName,
            submissionEntity = submissionEntityOpt,
            workflows = workflows ++ workflowFailures,
            status = SubmissionStatuses.Submitted,
            useCallCache = submissionRequest.useCallCache,
            workflowFailureMode = workflowFailureMode
          )

          // implicitly passed to SubmissionComponent.create
          implicit val subStatusCounter = submissionStatusCounter(workspaceMetricBuilder(workspaceName))
          implicit val wfStatusCounter = (status: WorkflowStatus) =>
            if (config.trackDetailedSubmissionMetrics) Option(workflowStatusCounter(workspaceSubmissionMetricBuilder(workspaceName, submissionId))(status))
            else None

          dataAccess.submissionQuery.create(workspaceContext, submission) map { _ =>
            RequestComplete(StatusCodes.Created, SubmissionReport(submissionRequest, submission.submissionId, submission.submissionDate, userInfo.userEmail.value, submission.status, header, successes))
          }
        }
    }
  }

  def validateSubmission(workspaceName: WorkspaceName, submissionRequest: SubmissionRequest): Future[PerRequestMessage] =
    withSubmissionParameters(workspaceName,submissionRequest) {
      (dataAccess: DataAccess, workspaceContext: SlickWorkspaceContext, wdl: String, header: SubmissionValidationHeader, succeeded: Seq[SubmissionValidationEntityInputs], failed: Seq[SubmissionValidationEntityInputs], _) =>
        DBIO.successful(RequestComplete(StatusCodes.OK, SubmissionValidationReport(submissionRequest, header, succeeded, failed)))
    }

  def getSubmissionStatus(workspaceName: WorkspaceName, submissionId: String): Future[PerRequestMessage] = {
    val submissionWithoutCosts = dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withSubmission(workspaceContext, submissionId, dataAccess) { submission =>
            DBIO.successful(submission)
        }
      }
    }

    submissionWithoutCosts flatMap {
      case (submission) => {
        val allWorkflowIds: Seq[String] = submission.workflows.flatMap(_.workflowId)
        toFutureTry(submissionCostService.getSubmissionCosts(submissionId, allWorkflowIds, workspaceName.namespace, Option(submission.submissionDate))) map {
          case Failure(ex) =>
            logger.error("Unable to get workflow costs for this submission. ", ex)
            RequestComplete((StatusCodes.OK, SubmissionStatusResponse(submission)))
          case Success(costMap) =>
            val costedWorkflows = submission.workflows.map { workflow =>
              workflow.workflowId match {
                case Some(wfId) => workflow.copy(cost = costMap.get(wfId))
                case None => workflow
              }
            }
            val costedSubmission = submission.copy(cost = Some(costMap.values.sum), workflows = costedWorkflows)
            RequestComplete((StatusCodes.OK, SubmissionStatusResponse(costedSubmission)))
        }
      }
    }
  }

  def abortSubmission(workspaceName: WorkspaceName, submissionId: String): Future[PerRequestMessage] = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
        abortSubmission(workspaceContext, submissionId, dataAccess)
      }
    }
  }

  private def abortSubmission(workspaceContext: SlickWorkspaceContext, submissionId: String, dataAccess: DataAccess): ReadWriteAction[PerRequestMessage] = {
    withSubmissionId(workspaceContext, submissionId, dataAccess) { submissionId =>
      // implicitly passed to SubmissionComponent.updateStatus
      implicit val subStatusCounter = submissionStatusCounter(workspaceMetricBuilder(workspaceContext.workspace.toWorkspaceName))
      dataAccess.submissionQuery.updateStatus(submissionId, SubmissionStatuses.Aborting) map { rows =>
        if(rows == 1)
          RequestComplete(StatusCodes.NoContent)
        else
          RequestComplete(ErrorReport(StatusCodes.NotFound, s"Unable to abort submission. Submission ${submissionId} could not be found."))
      }
    }
  }

  /**
   * Munges together the output of Cromwell's /outputs and /logs endpoints, grouping them by task name */
  private def mergeWorkflowOutputs(execOuts: ExecutionServiceOutputs, execLogs: ExecutionServiceLogs, workflowId: String): PerRequestMessage = {
    val outs = execOuts.outputs
    val logs = execLogs.calls getOrElse Map()

    //Cromwell workflow outputs look like workflow_name.task_name.output_name.
    //Under perverse conditions it might just be workflow_name.output_name.
    //Group outputs by everything left of the rightmost dot.
    val outsByTask = outs groupBy { case (k,_) => k.split('.').dropRight(1).mkString(".") }

    val taskMap = (outsByTask.keySet ++ logs.keySet).map( key => key -> TaskOutput( logs.get(key), outsByTask.get(key)) ).toMap
    RequestComplete(StatusCodes.OK, WorkflowOutputs(workflowId, taskMap))
  }

  /**
   * Get the list of outputs for a given workflow in this submission */
  def workflowOutputs(workspaceName: WorkspaceName, submissionId: String, workflowId: String) = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withWorkflowRecord(workspaceName, submissionId, workflowId, dataAccess) { wr =>
          val outputFTs = toFutureTry(executionServiceCluster.outputs(wr, userInfo))
          val logFTs = toFutureTry(executionServiceCluster.logs(wr, userInfo))
          DBIO.from(outputFTs zip logFTs map {
            case (Success(outputs), Success(logs)) =>
              mergeWorkflowOutputs(outputs, logs, workflowId)
            case (Failure(outputsFailure), Success(logs)) =>
              throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadGateway, s"Unable to get outputs for ${submissionId}.", executionServiceCluster.toErrorReport(outputsFailure)))
            case (Success(outputs), Failure(logsFailure)) =>
              throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadGateway, s"Unable to get logs for ${submissionId}.", executionServiceCluster.toErrorReport(logsFailure)))
            case (Failure(outputsFailure), Failure(logsFailure)) =>
              throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadGateway, s"Unable to get outputs and unable to get logs for ${submissionId}.",
                Seq(executionServiceCluster.toErrorReport(outputsFailure),executionServiceCluster.toErrorReport(logsFailure))))
          })
        }
      }
    }
  }

  // retrieve the cost of this Workflow from BigQuery, if available
  def workflowCost(workspaceName: WorkspaceName, submissionId: String, workflowId: String) = {

    // confirm: the user can Read this Workspace, the Submission is in this Workspace,
    // and the Workflow is in the Submission

    val execIdFutOpt = dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withSubmissionAndWorkflowExecutionServiceKey(workspaceContext, submissionId, workflowId, dataAccess) { optExecKey =>
          withSubmission(workspaceContext, submissionId, dataAccess) { submission =>
            DBIO.successful((optExecKey, submission))
          }
        }
      }
    }

    for {
      (optExecId, submission) <- execIdFutOpt
      // we don't need the Execution Service ID, but we do need to confirm the Workflow is in one for this Submission
      // if we weren't able to do so above
      _ <- executionServiceCluster.findExecService(submissionId, workflowId, userInfo, optExecId)
      costs <- submissionCostService.getWorkflowCost(workflowId, workspaceName.namespace, Option(submission.submissionDate))
    } yield RequestComplete(StatusCodes.OK, WorkflowCost(workflowId, costs.get(workflowId)))
  }

  def workflowMetadata(workspaceName: WorkspaceName, submissionId: String, workflowId: String, metadataParams: MetadataParams): Future[PerRequestMessage] = {

    // two possibilities here:
    //
    // (classic case) if the workflow is a top-level workflow of a submission, it has a row in the DB and an
    // association with a specific execution service shard.  Use the DB to verify the submission association and retrieve
    // the execution service identifier.
    //
    // if it's a subworkflow (or sub-sub-workflow, etc) it's not present in the Rawls DB and we don't know which
    // execution service shard has processed it.  Query all* execution service shards for the workflow to learn its
    // submission association and which shard processed it.
    //
    // * in practice, one shard does everything except for some older workflows on shard 2.  Revisit this if that changes!

    // determine which case this is, and close the DB transaction
    val execIdFutOpt: Future[Option[ExecutionServiceId]] = dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
        withSubmissionAndWorkflowExecutionServiceKey(workspaceContext, submissionId, workflowId, dataAccess) { optExecKey =>
          DBIO.successful(optExecKey)
        }
      }
    }

    // query the execution service(s) for the metadata
    execIdFutOpt flatMap {
      executionServiceCluster.callLevelMetadata(submissionId, workflowId, metadataParams, _, userInfo)
    } map {
      metadata =>
        RequestComplete(StatusCodes.OK, metadata)
    }
  }

  def workflowQueueStatus() = {
    dataSource.inTransaction { dataAccess =>
      dataAccess.workflowQuery.countWorkflowsByQueueStatus.flatMap { statusMap =>
        // determine the current size of the workflow queue
        statusMap.get(WorkflowStatuses.Queued.toString) match {
          case Some(x) if x > 0 =>
            for {
              timeEstimate <- dataAccess.workflowAuditStatusQuery.queueTimeMostRecentSubmittedWorkflow
              workflowsAhead <- dataAccess.workflowQuery.countWorkflowsAheadOfUserInQueue(userInfo)
            } yield {
              RequestComplete(StatusCodes.OK, WorkflowQueueStatusResponse(timeEstimate, workflowsAhead, statusMap))
            }
          case _ => DBIO.successful(RequestComplete(StatusCodes.OK, WorkflowQueueStatusResponse(0, 0, statusMap)))
        }
      }
    }
  }

  def adminWorkflowQueueStatusByUser() = {
    asFCAdmin {
      dataSource.inTransaction ({ dataAccess =>
        for {
          global <- dataAccess.workflowQuery.countWorkflowsByQueueStatus
          perUser <- dataAccess.workflowQuery.countWorkflowsByQueueStatusByUser
        } yield RequestComplete(StatusCodes.OK, WorkflowQueueStatusByUserResponse(global, perUser, maxActiveWorkflowsTotal, maxActiveWorkflowsPerUser))
      }, TransactionIsolation.ReadUncommitted)
    }
  }

  /*
   If the user only has read access, check the bucket using the default pet.
   If the user has a higher level of access, check the bucket using the pet for this workspace's project.

   We use the default pet when possible because the default pet is created in a per-user shell project, i.e. not in
     this workspace's project. This prevents proliferation of service accounts within this workspace's project. For
     FireCloud's common read-only public workspaces, this is an important safeguard; else those common projects
     would constantly hit limits on the number of allowed service accounts.

   If the user has write access, we need to use the pet for this workspace's project in order to get accurate results.
 */
  def checkBucketReadAccess(workspaceName: WorkspaceName) = {
    for {
      (workspace, maxAccessLevel) <- dataSource.inTransaction { dataAccess =>
        withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.read, dataAccess) { workspaceContext =>
          DBIO.from(getMaximumAccessLevel(workspaceContext.workspaceId.toString)).map { accessLevel =>
            (workspaceContext.workspace, accessLevel)
          }
        }
      }

      petKey <- if (maxAccessLevel >= WorkspaceAccessLevels.Write)
        samDAO.getPetServiceAccountKeyForUser(workspace.namespace, userInfo.userEmail)
      else
        samDAO.getDefaultPetServiceAccountKeyForUser(userInfo)

      accessToken <- gcsDAO.getAccessTokenUsingJson(petKey)

      (petEmail, petSubjectId) = petKey.parseJson match {
        case JsObject(fields) => (RawlsUserEmail(fields("client_email").toString), RawlsUserSubjectId(fields("client_id").toString))
        case _ => throw new RawlsException("pet service account key was not a json object")
      }

      resultsForPet <- gcsDAO.diagnosticBucketRead(UserInfo(petEmail, OAuth2BearerToken(accessToken), 60, petSubjectId), workspace.bucketName)
    } yield {
      resultsForPet match {
        case None => RequestComplete(StatusCodes.OK)
        case Some(report) => RequestComplete(report)
      }
    }
  }

  def checkSamActionWithLock(workspaceName: WorkspaceName, samAction: SamResourceAction): Future[PerRequestMessage] = {
    val wsCtxFuture = dataSource.inTransaction { dataAccess =>
      withWorkspaceContext(workspaceName, dataAccess, Some(WorkspaceAttributeSpecs(all = false))) { workspaceContext =>
        DBIO.successful(workspaceContext)
      }
    }

    //don't do the sam REST call inside the db transaction.
    val access: Future[PerRequestMessage] = wsCtxFuture flatMap { workspaceContext =>
      requireAccessF(workspaceContext.workspace, samAction) {
        Future.successful(RequestComplete(StatusCodes.NoContent)) //if we get here, we passed all the hoops
      }
    }

    //if we failed for any reason, the user can't do that thing on the workspace
    access.recover { case _ =>
      RequestComplete(StatusCodes.Forbidden) }
  }

  def listAllActiveSubmissions() = {
    dataSource.inTransaction { dataAccess =>
      dataAccess.submissionQuery.listAllActiveSubmissions().map(RequestComplete(StatusCodes.OK, _))
    }
  }

  def adminAbortSubmission(workspaceName: WorkspaceName, submissionId: String) = {
    asFCAdmin {
      dataSource.inTransaction { dataAccess =>
        withWorkspaceContext(workspaceName, dataAccess) { workspaceContext =>
          abortSubmission(workspaceContext, submissionId, dataAccess)
        }
      }
    }
  }

  def listAllWorkspaces() = {
    asFCAdmin {
      dataSource.inTransaction { dataAccess =>
        dataAccess.workspaceQuery.listAll.map(workspaces => RequestComplete(StatusCodes.OK, workspaces.map(w => WorkspaceDetails(w, Set.empty))))
      }
    }
  }

  def listWorkspacesWithAttribute(attributeName: AttributeName, attributeValue: AttributeValue): Future[PerRequestMessage] = {
    for {
      workspaces <- dataSource.inTransaction { dataAccess =>
        dataAccess.workspaceQuery.listWithAttribute(attributeName, attributeValue)
      }
      results <- Future.traverse(workspaces) { workspace =>
        loadResourceAuthDomain(SamResourceTypeNames.workspace, workspace.workspaceId, userInfo).map(WorkspaceDetails(workspace, _))
      }
    } yield {
      RequestComplete(StatusCodes.OK, results)
    }

  }

  def getBucketUsage(workspaceName: WorkspaceName): Future[PerRequestMessage] = {
    for {
      bucketName <- dataSource.inTransaction { dataAccess =>
        withWorkspaceContext(workspaceName, dataAccess) { workspaceContext =>
          requireAccessIgnoreLock(workspaceContext.workspace, SamWorkspaceActions.write) {
            DBIO.successful(workspaceContext.workspace.bucketName)
          }
        }
      }
      usage <- gcsDAO.getBucketUsage(RawlsBillingProjectName(workspaceName.namespace), bucketName)
    } yield {
      RequestComplete(BucketUsageResponse(usage))
    }
  }

  def getAccessInstructions(workspaceName: WorkspaceName): Future[PerRequestMessage] = {
    for {
      workspaceId <- loadWorkspaceId(workspaceName)
      authDomains <- samDAO.getResourceAuthDomain(SamResourceTypeNames.workspace, workspaceId, userInfo)
      instructions <- Future.traverse(authDomains) { adGroup =>
        samDAO.getAccessInstructions(WorkbenchGroupName(adGroup), userInfo).map { maybeInstruction =>
          maybeInstruction.map(i => ManagedGroupAccessInstructions(adGroup, i))
        }
      }
    } yield RequestComplete(StatusCodes.OK, instructions.flatten)
  }

  def getGenomicsOperationV2(workflowId: String, operationId: List[String]): Future[PerRequestMessage] = {
    // note that cromiam should only give back metadata if the user is authorized to see it
    cromiamDAO.callLevelMetadata(workflowId, MetadataParams(includeKeys = Set("jobId")), userInfo).flatMap { metadataJson =>
      val operationIds: Iterable[String] = WorkspaceService.extractOperationIdsFromCromwellMetadata(metadataJson)

      val operationIdString = operationId.mkString("/")
      // check that the requested operation id actually exists in the workflow
      if (operationIds.toList.contains(operationIdString)) {
        val genomicsServiceRef = genomicsServiceConstructor(userInfo)
        genomicsServiceRef.GetOperation(operationIdString)
      } else {
        Future.failed(new RawlsExceptionWithErrorReport(ErrorReport(StatusCodes.NotFound, s"operation id ${operationIdString} not found in workflow $workflowId")))
      }
    }
  }

  // helper methods

  // note: success is indicated by  Map.empty
  private def attributeNamespaceCheck(attributeNames: Iterable[AttributeName]): Map[String, String] = {
    val namespaces = attributeNames.map(_.namespace).toSet

    // no one can modify attributes with invalid namespaces
    val invalidNamespaces = namespaces -- AttributeName.validNamespaces
    invalidNamespaces.map { ns => ns -> s"Invalid attribute namespace $ns" }.toMap
  }

  private def withAttributeNamespaceCheck[T](attributeNames: Iterable[AttributeName])(op: => T): T = {
    val errors = attributeNamespaceCheck(attributeNames)
    if (errors.isEmpty) op
    else {
      val reasons = errors.values.mkString(", ")
      val err = ErrorReport(statusCode = StatusCodes.Forbidden, message = s"Attribute namespace validation failed: [$reasons]")
      throw new RawlsExceptionWithErrorReport(errorReport = err)
    }
  }

  private def withAttributeNamespaceCheck[T](hasAttributes: Attributable)(op: => Future[T]): Future[T] =
    withAttributeNamespaceCheck(hasAttributes.attributes.keys)(op)

  private def withAttributeNamespaceCheck[T](methodConfiguration: MethodConfiguration)(op: => Future[T]): Future[T] = {
    // TODO: this duplicates expression parsing, the canonical way to do this.  Use that instead?
    // valid method configuration outputs are either in the format this.attrname or workspace.attrname
    // invalid (unparseable) will be caught by expression parsing instead
    val attrNames = methodConfiguration.outputs map { case (_, attr) => AttributeName.fromDelimitedName(attr.value.split('.').last) }
    withAttributeNamespaceCheck(attrNames)(op)
  }

  private def createWorkflowCollectionForWorkspace(workspaceId: String, policyMap: Map[SamResourcePolicyName, WorkbenchEmail], parentSpan: Span = null) = {
    for {
      _ <- traceWithParent("createResourceFull",parentSpan)( _ => samDAO.createResourceFull(
              SamResourceTypeNames.workflowCollection,
              workspaceId,
              Map(
                SamWorkflowCollectionPolicyNames.workflowCollectionOwnerPolicyName ->
                  SamPolicy(Set(policyMap(SamWorkspacePolicyNames.projectOwner), policyMap(SamWorkspacePolicyNames.owner)), Set.empty, Set(SamWorkflowCollectionRoles.owner)),
                SamWorkflowCollectionPolicyNames.workflowCollectionWriterPolicyName ->
                  SamPolicy(Set(policyMap(SamWorkspacePolicyNames.canCompute)), Set.empty, Set(SamWorkflowCollectionRoles.writer)),
                SamWorkflowCollectionPolicyNames.workflowCollectionReaderPolicyName ->
                  SamPolicy(Set(policyMap(SamWorkspacePolicyNames.reader), policyMap(SamWorkspacePolicyNames.writer)), Set.empty, Set(SamWorkflowCollectionRoles.reader))
              ),
              Set.empty,
              userInfo
            ))
    } yield {
    }
  }

  private def withNewWorkspaceContext[T](workspaceRequest: WorkspaceRequest, dataAccess: DataAccess, parentSpan: Span = null)
                                     (op: (SlickWorkspaceContext) => ReadWriteAction[T]): ReadWriteAction[T] = {

    def getBucketName(workspaceId: String, secure: Boolean) = s"${config.workspaceBucketNamePrefix}-${if(secure) "secure-" else ""}${workspaceId}"
    def getLabels(authDomain: List[ManagedGroupRef]) = authDomain match {
      case Nil => Map(WorkspaceService.SECURITY_LABEL_KEY -> WorkspaceService.LOW_SECURITY_LABEL)
      case ads => Map(WorkspaceService.SECURITY_LABEL_KEY -> WorkspaceService.HIGH_SECURITY_LABEL) ++ ads.map(ad => gcsDAO.labelSafeString(ad.membersGroupName.value, "ad-") -> "")
    }

    def saveNewWorkspace(workspaceId: String, workspaceRequest: WorkspaceRequest, bucketName: String, projectOwnerPolicyEmail: WorkbenchEmail, dataAccess: DataAccess, parentSpan: Span = null): ReadWriteAction[(Workspace, Map[SamResourcePolicyName, WorkbenchEmail])] = {
      val currentDate = DateTime.now

      val workspace = Workspace(
        namespace = workspaceRequest.namespace,
        name = workspaceRequest.name,
        workspaceId = workspaceId,
        bucketName = bucketName,
        workflowCollectionName = Some(workspaceId),
        createdDate = currentDate,
        lastModified = currentDate,
        createdBy = userInfo.userEmail.value,
        attributes = workspaceRequest.attributes
      )

      traceDBIOWithParent("save", parentSpan)(_ => dataAccess.workspaceQuery.save(workspace)).flatMap { _ =>
        DBIO.from(for {
          resource <- {
            val projectOwnerPolicy = SamWorkspacePolicyNames.projectOwner -> SamPolicy(Set(projectOwnerPolicyEmail), Set.empty, Set(SamWorkspaceRoles.owner, SamWorkspaceRoles.projectOwner))
            val ownerPolicy = SamWorkspacePolicyNames.owner -> SamPolicy(Set(WorkbenchEmail(userInfo.userEmail.value)), Set.empty, Set(SamWorkspaceRoles.owner))
            val writerPolicy = SamWorkspacePolicyNames.writer -> SamPolicy(Set.empty, Set.empty, Set(SamWorkspaceRoles.writer))
            val readerPolicy = SamWorkspacePolicyNames.reader -> SamPolicy(Set.empty, Set.empty, Set(SamWorkspaceRoles.reader))
            val shareReaderPolicy = SamWorkspacePolicyNames.shareReader -> SamPolicy(Set.empty, Set.empty, Set(SamWorkspaceRoles.shareReader))
            val shareWriterPolicy = SamWorkspacePolicyNames.shareWriter -> SamPolicy(Set.empty, Set.empty, Set(SamWorkspaceRoles.shareWriter))
            val canComputePolicy = SamWorkspacePolicyNames.canCompute -> SamPolicy(Set.empty, Set.empty, Set(SamWorkspaceRoles.canCompute))
            val canCatalogPolicy = SamWorkspacePolicyNames.canCatalog -> SamPolicy(Set.empty, Set.empty, Set(SamWorkspaceRoles.canCatalog))

            val defaultPolicies = Map(projectOwnerPolicy, ownerPolicy, writerPolicy, readerPolicy, shareReaderPolicy, shareWriterPolicy, canComputePolicy, canCatalogPolicy)

            traceWithParent("createResourceFull", parentSpan)(_ => samDAO.createResourceFull(SamResourceTypeNames.workspace, workspaceId, defaultPolicies, workspaceRequest.authorizationDomain.getOrElse(Set.empty).map(_.membersGroupName.value), userInfo))
          }

          // policyMap has policyName -> policyEmail
          policyMap: Map[SamResourcePolicyName, WorkbenchEmail] = resource.accessPolicies.map( x => SamResourcePolicyName(x.id.accessPolicyName) -> WorkbenchEmail(x.email)).toMap

          // declare these next two Futures so they start in parallel
          createWorkflowCollectionFuture = traceWithParent("createWorkflowCollectionForWorkspace", parentSpan)( s1 => createWorkflowCollectionForWorkspace(workspaceId, policyMap, s1))

          traversePolicies = traceWithParent("traversePolicies", parentSpan)( s1 =>
            Future.traverse(policyMap) { x =>
              val policyName = x._1
              if (policyName == SamWorkspacePolicyNames.projectOwner && workspaceRequest.authorizationDomain.getOrElse(Set.empty).isEmpty) {
                // when there isn't an auth domain, we will use the billing project admin policy email directly on workspace
                // resources instead of synching an extra group. This helps to keep the number of google groups a user is in below
                // the limit of 2000
                Future.successful(())
              } else if (WorkspaceAccessLevels.withPolicyName(policyName.value).isDefined) {
                // only sync policies that have corresponding WorkspaceAccessLevels to google because only those are
                // granted bucket access (and thus need a google group)
                traceWithParent(s"syncPolicy-${policyName}", s1)( _ => samDAO.syncPolicyToGoogle(SamResourceTypeNames.workspace, workspaceId, policyName))
              } else {
                Future.successful(())
              }
            }
          )

          _ <- createWorkflowCollectionFuture

          _ <- traversePolicies

        } yield (workspace, policyMap))
      }
    }


    traceDBIOWithParent("requireCreateWorkspaceAccess", parentSpan)( s1 => requireCreateWorkspaceAccess(workspaceRequest, dataAccess, s1) {
      traceDBIOWithParent("findByName", s1)( _=> dataAccess.workspaceQuery.findByName(workspaceRequest.toWorkspaceName, Option(WorkspaceAttributeSpecs(all = false, List.empty[AttributeName])))) flatMap {
        case Some(_) => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Conflict, s"Workspace ${workspaceRequest.namespace}/${workspaceRequest.name} already exists")))
        case None =>
          val workspaceId = UUID.randomUUID.toString
          val bucketName = getBucketName(workspaceId, workspaceRequest.authorizationDomain.exists(_.nonEmpty))

          // add the workspace id to the span so we can find and correlate it later with other services
          s1.putAttribute("workspaceId", OpenCensusAttributeValue.stringAttributeValue(workspaceId))

          traceDBIOWithParent("getPolicySyncStatus", s1)( _ => DBIO.from(samDAO.getPolicySyncStatus(SamResourceTypeNames.billingProject, workspaceRequest.namespace, SamBillingProjectPolicyNames.owner, userInfo).map(_.email))).flatMap { projectOwnerPolicyEmail =>
            traceDBIOWithParent("saveNewWorkspace", s1)(s2 => saveNewWorkspace(workspaceId, workspaceRequest, bucketName, projectOwnerPolicyEmail, dataAccess, s2).flatMap { case (savedWorkspace, policyMap) =>
              for {
                //there's potential for another perf improvement here for workspaces with auth domains. if a workspace is in an auth domain, we'll already have
                //the projectOwnerEmail, so we don't need to get it from sam. in a pinch, we could also store the project owner email in the rawls DB since it
                //will never change, which would eliminate the call to sam entirely
                policyEmails <- DBIO.successful(policyMap.map { case (policyName, policyEmail) =>
                  if (policyName == SamWorkspacePolicyNames.projectOwner && workspaceRequest.authorizationDomain.getOrElse(Set.empty).isEmpty) {
                    // when there isn't an auth domain, we will use the billing project admin policy email directly on workspace
                    // resources instead of synching an extra group. This helps to keep the number of google groups a user is in below
                    // the limit of 2000
                    Option(WorkspaceAccessLevels.ProjectOwner -> projectOwnerPolicyEmail)
                  } else {
                    WorkspaceAccessLevels.withPolicyName(policyName.value).map(_ -> policyEmail)
                }
                  }.flatten.toMap )

                _ <- traceDBIOWithParent("gcsDAO.setupWorkspace", s2)(s3 => DBIO.from(gcsDAO.setupWorkspace(userInfo, RawlsBillingProjectName(workspaceRequest.namespace), policyEmails, bucketName, getLabels(workspaceRequest.authorizationDomain.getOrElse(Set.empty).toList), s3)))
                response <- traceDBIOWithParent("doOp", s2)(_ => op(SlickWorkspaceContext(savedWorkspace)))
              } yield response
            })
          }
      }
    })
  }

  private def noSuchWorkspaceMessage(workspaceName: WorkspaceName) = s"${workspaceName} does not exist"
  private def accessDeniedMessage(workspaceName: WorkspaceName) = s"insufficient permissions to perform operation on ${workspaceName}"

  private def requireCreateWorkspaceAccess[T](workspaceRequest: WorkspaceRequest, dataAccess: DataAccess, parentSpan: Span = null)(op: => ReadWriteAction[T]): ReadWriteAction[T] = {
    val projectName = RawlsBillingProjectName(workspaceRequest.namespace)
    for {
      userHasAction <- traceDBIOWithParent("userHasAction", parentSpan)(_ => DBIO.from(samDAO.userHasAction(SamResourceTypeNames.billingProject, projectName.value, SamBillingProjectActions.createWorkspace, userInfo)))
      response <- userHasAction match {
        case true =>
          traceDBIOWithParent("loadBillingProject", parentSpan)( _ => dataAccess.rawlsBillingProjectQuery.load(projectName)).flatMap {
            case Some(RawlsBillingProject(_, _, CreationStatuses.Ready, _, _, _, _, _)) => op //Sam will check to make sure the Auth Domain selection is valid
            case Some(RawlsBillingProject(RawlsBillingProjectName(name), _, CreationStatuses.Creating, _, _, _, _, _)) =>
              DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"${name} is still being created")))

            case Some(RawlsBillingProject(RawlsBillingProjectName(name), _, CreationStatuses.Error, _, messageOp, _, _, _)) =>
              DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"Error creating ${name}: ${messageOp.getOrElse("no message")}")))

            case None =>
              // this can't happen with the current code but a 404 would be the correct response
              DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"${workspaceRequest.toWorkspaceName.namespace} does not exist")))
          }
        case false =>
          DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Forbidden, s"You are not authorized to create a workspace in billing project ${workspaceRequest.toWorkspaceName.namespace}")))
      }
    } yield response
  }

  private def withWorkspaceContextAndPermissions[T](workspaceName: WorkspaceName, requiredAction: SamResourceAction, dataAccess: DataAccess)(op: (SlickWorkspaceContext) => ReadWriteAction[T]): ReadWriteAction[T] = {
    withWorkspaceContext(workspaceName, dataAccess) { workspaceContext =>
      requireAccess(workspaceContext.workspace, requiredAction) { op(workspaceContext) }
    }
  }

  private def withWorkspaceContext[T](workspaceName: WorkspaceName, dataAccess: DataAccess, attributeSpecs: Option[WorkspaceAttributeSpecs] = None)(op: (SlickWorkspaceContext) => ReadWriteAction[T]) = {
    dataAccess.workspaceQuery.findByName(workspaceName, attributeSpecs) flatMap {
      case None => throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, noSuchWorkspaceMessage(workspaceName)))
      case Some(workspace) => op(SlickWorkspaceContext(workspace))
    }
  }

  private def accessCheck(workspace: Workspace, requiredAction: SamResourceAction): Future[Unit] = {
    samDAO.userHasAction(SamResourceTypeNames.workspace, workspace.workspaceId, requiredAction, userInfo) flatMap { hasRequiredLevel =>
      if (hasRequiredLevel) {
        if (Set(SamWorkspaceActions.write, SamWorkspaceActions.compute).contains(requiredAction) && workspace.isLocked)
          Future.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Forbidden, s"The workspace ${workspace.toWorkspaceName} is locked.")))
        else
          Future.successful(())
      } else {
        samDAO.userHasAction(SamResourceTypeNames.workspace, workspace.workspaceId, SamWorkspaceActions.read, userInfo) flatMap { canRead =>
          if (canRead) {
            Future.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Forbidden, accessDeniedMessage(workspace.toWorkspaceName))))
          }
          else {
            Future.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, noSuchWorkspaceMessage(workspace.toWorkspaceName))))
          }
        }
      }
    }
  }

  private def requireAccess[T](workspace: Workspace, requiredAction: SamResourceAction)(codeBlock: => ReadWriteAction[T]): ReadWriteAction[T] = {
    DBIO.from(accessCheck(workspace, requiredAction)) flatMap { _ => codeBlock }
  }

  private def requireAccessF[T](workspace: Workspace, requiredAction: SamResourceAction)(codeBlock: => Future[T]): Future[T] = {
    accessCheck(workspace, requiredAction) flatMap { _ => codeBlock }
  }

  private def requireAccessIgnoreLock[T](workspace: Workspace, requiredAction: SamResourceAction)(op: => ReadWriteAction[T]): ReadWriteAction[T] = {
    requireAccess(workspace.copy(isLocked = false), requiredAction)(op)
  }

  private def requireComputePermission[T](workspace: Workspace)(codeBlock: => ReadWriteAction[T]): ReadWriteAction[T] = {
    DBIO.from(samDAO.userHasAction(SamResourceTypeNames.billingProject, workspace.namespace, SamBillingProjectActions.launchBatchCompute, userInfo)) flatMap { projectCanCompute =>
      if (!projectCanCompute) DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Forbidden, accessDeniedMessage(workspace.toWorkspaceName))))
      else {
        DBIO.from(samDAO.userHasAction(SamResourceTypeNames.workspace, workspace.workspaceId, SamWorkspaceActions.compute, userInfo)) flatMap { launchBatchCompute =>
          if (launchBatchCompute) codeBlock
          else DBIO.from(samDAO.userHasAction(SamResourceTypeNames.workspace, workspace.workspaceId, SamWorkspaceActions.read, userInfo)) flatMap { workspaceRead =>
            if (workspaceRead) DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.Forbidden, accessDeniedMessage(workspace.toWorkspaceName))))
            else DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, noSuchWorkspaceMessage(workspace.toWorkspaceName))))
          }
        }
      }
    }
  }

  private def withEntity[T](workspaceContext: SlickWorkspaceContext, entityType: String, entityName: String, dataAccess: DataAccess)(op: (Entity) => ReadWriteAction[T]): ReadWriteAction[T] = {
    dataAccess.entityQuery.get(workspaceContext, entityType, entityName) flatMap {
      case None => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"${entityType} ${entityName} does not exist in ${workspaceContext.workspace.toWorkspaceName}")))
      case Some(entity) => op(entity)
    }
  }

  private def withAllEntities[T](workspaceContext: SlickWorkspaceContext, dataAccess: DataAccess, entities: Seq[AttributeEntityReference])(op: (Seq[Entity]) => ReadWriteAction[T]): ReadWriteAction[T] = {
    val entityActions: Seq[ReadAction[Try[Entity]]] = entities map { e =>
      dataAccess.entityQuery.get(workspaceContext, e.entityType, e.entityName) map {
        case None => Failure(new RawlsException(s"${e.entityType} ${e.entityName} does not exist in ${workspaceContext.workspace.toWorkspaceName}"))
        case Some(entity) => Success(entity)
      }
    }

    DBIO.sequence(entityActions) flatMap { entityTries =>
      val failures = entityTries.collect { case Failure(y) => y.getMessage }
      if (failures.isEmpty) op(entityTries collect { case Success(e) => e })
      else {
        val err = ErrorReport(statusCode = StatusCodes.BadRequest, message = (Seq("Entities were not found:") ++ failures) mkString System.lineSeparator())
        DBIO.failed(new RawlsExceptionWithErrorReport(err))
      }
    }
  }

  private def withSubmission[T](workspaceContext: SlickWorkspaceContext, submissionId: String, dataAccess: DataAccess)(op: (Submission) => ReadWriteAction[T]): ReadWriteAction[T] = {
    Try {
      UUID.fromString(submissionId)
    } match {
      case Failure(t: IllegalArgumentException) =>
        DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"Submission id ${submissionId} is not a valid submission id")))
      case _ =>
        dataAccess.submissionQuery.get(workspaceContext, submissionId) flatMap {
          case None => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"Submission with id ${submissionId} not found in workspace ${workspaceContext.workspace.toWorkspaceName}")))
          case Some(submission) => op(submission)
        }
    }
  }

  // confirm that the Submission is a member of this workspace, but don't unmarshal it from the DB
  private def withSubmissionId[T](workspaceContext: SlickWorkspaceContext, submissionId: String, dataAccess: DataAccess)(op: UUID => ReadWriteAction[T]): ReadWriteAction[T] = {
    Try {
      UUID.fromString(submissionId)
    } match {
      case Failure(t: IllegalArgumentException) =>
        DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"Submission id ${submissionId} is not a valid submission id")))
      case Success(uuid) =>
        dataAccess.submissionQuery.confirmInWorkspace(workspaceContext.workspaceId, uuid) flatMap {
          case None =>
            val report = ErrorReport(StatusCodes.NotFound, s"Submission with id ${submissionId} not found in workspace ${workspaceContext.workspace.toWorkspaceName}")
            DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = report))
          case Some(_) => op(uuid)
        }
    }
  }

  private def withWorkflow(workspaceName: WorkspaceName, submissionId: String, workflowId: String, dataAccess: DataAccess)(op: (Workflow) => ReadWriteAction[PerRequestMessage]): ReadWriteAction[PerRequestMessage] = {
    dataAccess.workflowQuery.getByExternalId(workflowId, submissionId) flatMap {
      case None => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"Workflow with id ${workflowId} not found in submission ${submissionId} in workspace ${workspaceName}")))
      case Some(workflow) => op(workflow)
    }
  }

  private def withWorkflowRecord(workspaceName: WorkspaceName, submissionId: String, workflowId: String, dataAccess: DataAccess)(op: (WorkflowRecord) => ReadWriteAction[PerRequestMessage]): ReadWriteAction[PerRequestMessage] = {
    dataAccess.workflowQuery.findWorkflowByExternalIdAndSubmissionId(workflowId, UUID.fromString(submissionId)).result flatMap {
      case Seq() => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"WorkflowRecord with id ${workflowId} not found in submission ${submissionId} in workspace ${workspaceName}")))
      case Seq(one) => op(one)
      case tooMany => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.InternalServerError, s"found multiple WorkflowRecords with id ${workflowId} in submission ${submissionId} in workspace ${workspaceName}")))
    }
  }

  // used as part of the workflow metadata permission check - more detail at workflowMetadata()

  // require submission to be present, but don't require the workflow to reference it
  // if the workflow does reference the submission, return its executionServiceKey

  private def withSubmissionAndWorkflowExecutionServiceKey[T](workspaceContext: SlickWorkspaceContext, submissionId: String, workflowId: String, dataAccess: DataAccess)(op: Option[ExecutionServiceId] => ReadWriteAction[T]): ReadWriteAction[T] = {
    withSubmissionId(workspaceContext, submissionId, dataAccess) { _ =>
      dataAccess.workflowQuery.getExecutionServiceIdByExternalId(workflowId, submissionId) flatMap {
        case Some(id) => op(Option(ExecutionServiceId(id)))
        case _ => op(None)
      }
    }
  }

  //Finds a single entity record in the db.
  private def withSingleEntityRec(entityType: String, entityName: String, workspaceContext: SlickWorkspaceContext, dataAccess: DataAccess)(op: (Seq[EntityRecord]) => ReadWriteAction[PerRequestMessage]): ReadWriteAction[PerRequestMessage] = {
    val entityRec = dataAccess.entityQuery.findEntityByName(workspaceContext.workspaceId, entityType, entityName).result
    entityRec flatMap { entities =>
      if (entities.isEmpty) {
        DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"No entity of type ${entityType} named ${entityName} exists in this workspace.")))
      } else if (entities.size == 1) {
        op(entities)
      } else {
        DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.NotFound, s"More than one entity of type ${entityType} named ${entityName} exists in this workspace?!")))
      }
    }
  }

  def withSubmissionEntityRecs(submissionRequest: SubmissionRequest, workspaceContext: SlickWorkspaceContext, rootEntityTypeOpt: Option[String], dataAccess: DataAccess)(op: (Option[Seq[EntityRecord]]) => ReadWriteAction[PerRequestMessage]): ReadWriteAction[PerRequestMessage] = {
    if( rootEntityTypeOpt.isEmpty ) {
      op(None)
    } else {
      val rootEntityType = rootEntityTypeOpt.get

      //If there's an expression, evaluate it to get the list of entities to run this job on.
      //Otherwise, use the entity given in the submission.
      submissionRequest.expression match {
        case None =>
          if (submissionRequest.entityType.getOrElse("") != rootEntityType) {
            val whatYouGaveUs = if (submissionRequest.entityType.isDefined) s"an entity of type ${submissionRequest.entityType.get}" else "no entity"
            DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"Method configuration expects an entity of type $rootEntityType, but you gave us $whatYouGaveUs.")))
          } else {
            withSingleEntityRec(submissionRequest.entityType.get, submissionRequest.entityName.get, workspaceContext, dataAccess)(rec => op(Some(rec)))
          }
        case Some(expression) =>
          ExpressionEvaluator.withNewExpressionEvaluator(dataAccess, workspaceContext, submissionRequest.entityType.get, submissionRequest.entityName.get) { evaluator =>
            evaluator.evalFinalEntity(workspaceContext, expression).asTry
          } flatMap { //gotta close out the expression evaluator to wipe the EXPREVAL_TEMP table
            case Failure(regret) => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, regret)))
            case Success(entityRecords) =>
              if (entityRecords.isEmpty) {
                DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, "No entities eligible for submission were found.")))
              } else {
                val eligibleEntities = entityRecords.filter(_.entityType == rootEntityType).toSeq
                if (eligibleEntities.isEmpty)
                  DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"The expression in your SubmissionRequest matched only entities of the wrong type. (Expected type ${rootEntityType}.)")))
                else
                  op(Some(eligibleEntities))
              }
          }
      }
    }
  }

  /** Validates the workflow failure mode in the submission request. */
  private def withWorkflowFailureMode(submissionRequest: SubmissionRequest)(op: Option[WorkflowFailureMode] => ReadWriteAction[PerRequestMessage]): ReadWriteAction[PerRequestMessage] = {
    Try(submissionRequest.workflowFailureMode.map(WorkflowFailureModes.withName)) match {
      case Success(failureMode) => op(failureMode)
      case Failure(NonFatal(e)) => DBIO.failed(new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, e.getMessage)))
    }
  }


  private def withSubmissionParameters(workspaceName: WorkspaceName, submissionRequest: SubmissionRequest)
    (op: (DataAccess, SlickWorkspaceContext, String, SubmissionValidationHeader, Seq[SubmissionValidationEntityInputs], Seq[SubmissionValidationEntityInputs], Option[WorkflowFailureMode]) => ReadWriteAction[PerRequestMessage]): Future[PerRequestMessage] = {
    dataSource.inTransaction { dataAccess =>
      withWorkspaceContextAndPermissions(workspaceName, SamWorkspaceActions.write, dataAccess) { workspaceContext =>
        withMethodConfig(workspaceContext, submissionRequest.methodConfigurationNamespace, submissionRequest.methodConfigurationName, dataAccess) { methodConfig =>
          withMethodInputs(methodConfig, userInfo) { (wdl, gatherInputsResult) =>

            //either both entityName and entityType must be defined, or neither. Error otherwise
            if(submissionRequest.entityName.isDefined != submissionRequest.entityType.isDefined) {
              throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"You must set both entityType and entityName to run on an entity, or neither (to run with literal or workspace inputs)."))
            }
            if(methodConfig.rootEntityType.isDefined != submissionRequest.entityName.isDefined) {
              if(methodConfig.rootEntityType.isDefined) {
                throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"Your method config defines a root entity but you haven't passed one to the submission."))
              } else {
                //This isn't _strictly_ necessary, since a single submission entity will create one workflow.
                //However, passing in a submission entity + an expression doesn't make sense for two reasons:
                // 1. you'd have to write an expression from your submission entity to an entity of "no entity necessary" type
                // 2. even if you _could_ do this, you'd kick off a bunch of identical workflows.
                //More likely than not, an MC with no root entity + a submission entity = you're doing something wrong. So we'll just say no here.
                throw new RawlsExceptionWithErrorReport(errorReport = ErrorReport(StatusCodes.BadRequest, s"Your method config uses no root entity, but you passed one to the submission."))
              }
            }
            withValidatedMCExpressions(methodConfig, gatherInputsResult, allowRootEntity = submissionRequest.entityName.isDefined, dataAccess) { _ =>
              withSubmissionEntityRecs(submissionRequest, workspaceContext, methodConfig.rootEntityType, dataAccess) { jobEntityRecs =>
                withWorkflowFailureMode(submissionRequest) { workflowFailureMode =>
                  //Parse out the entity -> results map to a tuple of (successful, failed) SubmissionValidationEntityInputs
                  methodConfigResolver.evaluateInputExpressions(workspaceContext, gatherInputsResult.processableInputs, jobEntityRecs, dataAccess) flatMap { valuesByEntity =>
                    valuesByEntity
                      .map({ case (entityName, values) => SubmissionValidationEntityInputs(entityName, values.toSet) })
                      .partition({ entityInputs => entityInputs.inputResolutions.forall(_.error.isEmpty) }) match {
                      case (succeeded, failed) =>
                        val methodConfigInputs = gatherInputsResult.processableInputs.map { methodInput => SubmissionValidationInput(methodInput.workflowInput.getName, methodInput.expression) }
                        val header = SubmissionValidationHeader(methodConfig.rootEntityType, methodConfigInputs)
                        op(dataAccess, workspaceContext, wdl, header, succeeded.toSeq, failed.toSeq, workflowFailureMode)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

class AttributeUpdateOperationException(message: String) extends RawlsException(message)
class AttributeNotFoundException(message: String) extends AttributeUpdateOperationException(message)

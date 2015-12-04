package org.broadinstitute.dsde.rawls.model

import org.broadinstitute.dsde.rawls.model.SubmissionStatuses.SubmissionStatus
import org.broadinstitute.dsde.rawls.model.WorkflowStatuses.WorkflowStatus
import UserAuthJsonSupport._

import spray.json._
import org.joda.time.DateTime
import org.broadinstitute.dsde.rawls.RawlsException

/**
 * @author tsharpe
 */

// this is what a client sends to have us create a submission

// Request for a submission
case class SubmissionRequest(
  methodConfigurationNamespace: String,
  methodConfigurationName: String,
  entityType: String,
  entityName: String,
  expression: Option[String]
)

// Cromwell's response to workflow submission
case class ExecutionServiceStatus(
  id: String,
  status: String
)

// Cromwell's response to workflow validation
case class ExecutionServiceValidation(
  valid: Boolean,
  error: String
)

case class ExecutionServiceOutputs(
  id: String,
  outputs: Map[String, Attribute]
)

case class ExecutionServiceLogs(
 id: String,
 logs: Map[String, Seq[Map[String,String]]]
)

case class ExecutionServiceWorkflowOptions(
  jes_gcs_root: String,
  google_project: String,
  account_name: String,
  refresh_token: String,
  auth_bucket: String)

case class TaskOutput(
  logs: Option[Seq[Map[String, String]]],
  outputs: Option[Map[String, Attribute]]
)

case class WorkflowOutputs(
  workflowId: String,
  tasks: Map[String, TaskOutput]
)

case class SubmissionStatusResponse(
  submissionId: String,
  submissionDate: DateTime,
  submitter: String,
  methodConfigurationNamespace: String,
  methodConfigurationName: String,
  submissionEntity: AttributeEntityReference,
  workflows: Seq[Workflow],
  notstarted: Seq[WorkflowFailure],
  status: SubmissionStatus
) {
  def this(submission: Submission, rawlsUser: RawlsUser) = this(submission.submissionId, submission.submissionDate, rawlsUser.userEmail.value, submission.methodConfigurationNamespace, submission.methodConfigurationName, submission.submissionEntity, submission.workflows, submission.notstarted, submission.status)
}

// method configuration input parameter, it's name and the associated expression from the method config
case class SubmissionValidationInput(
  wdlName: String,
  expression: String
)

// common values for all the entities -- the entity type and the input descriptions
case class SubmissionValidationHeader(
  entityType: String,
  inputExpressions: Seq[SubmissionValidationInput] // size of Seq is nInputs
)

// the results of parsing each of the inputs for one entity
case class SubmissionValidationEntityInputs(
  entityName: String,
  inputResolutions: Seq[SubmissionValidationValue] // size of Seq is nInputs
)

// the results of parsing each input for each entity
case class SubmissionValidationReport(
  request: SubmissionRequest,
  header: SubmissionValidationHeader,
  validEntities: Seq[SubmissionValidationEntityInputs], // entities for which parsing all inputs succeeded
  invalidEntities: Seq[SubmissionValidationEntityInputs] // entities for which parsing at least 1 of the inputs failed
)

case class WorkflowReport(
  workflowId: String,
  status: WorkflowStatus,
  statusLastChangedDate: DateTime,
  entityName: String,
  inputResolutions: Seq[SubmissionValidationValue] // size of Seq is nInputs
)

// the results of creating a submission
case class SubmissionReport(
  request: SubmissionRequest,
  submissionId: String,
  submissionDate: DateTime,
  submitter: String,
  status: SubmissionStatus,
  header: SubmissionValidationHeader,
  workflows: Seq[WorkflowReport],
  notstarted: Seq[SubmissionValidationEntityInputs]
)

case class CallMetadata(
  inputs: Map[String, Attribute],
  executionStatus: String,
  backend: Option[String],
  backendStatus: Option[String],
  outputs: Option[Map[String, Attribute]],
  start: Option[DateTime],
  end: Option[DateTime],
  jobId: Option[String],
  returnCode: Option[Int],
  shardIndex: Int,
  stdout: Option[String],
  stderr: Option[String]
)

case class ExecutionMetadata
(
  id: String,
  status: String,
  submission: DateTime,
  start: Option[DateTime],
  end: Option[DateTime],
  inputs: Map[String, String],
  outputs: Option[Map[String, String]],
  calls: Map[String, Seq[CallMetadata]]
)

case class ActiveSubmission
(
  workspaceNamespace: String,
  workspaceName: String,
  submission: Submission
)

object ExecutionJsonSupport extends JsonSupport {

  implicit object WorkflowStatusFormat extends RootJsonFormat[WorkflowStatus] {
    override def write(obj: WorkflowStatus): JsValue = JsString(obj.toString)
    override def read(json: JsValue): WorkflowStatus = json match {
      case JsString(name) => WorkflowStatuses.withName(name)
      case x => throw new DeserializationException("invalid value: " + x)
    }
  }

  implicit object SubmissionStatusFormat extends RootJsonFormat[SubmissionStatus] {
    override def write(obj: SubmissionStatus): JsValue = JsString(obj.toString)
    override def read(json: JsValue): SubmissionStatus = json match {
      case JsString(name) => SubmissionStatuses.withName(name)
      case x => throw new DeserializationException("invalid value: " + x)
    }
  }

  implicit val SubmissionRequestFormat = jsonFormat5(SubmissionRequest)

  implicit val ExecutionServiceStatusFormat = jsonFormat2(ExecutionServiceStatus)

  implicit val ExecutionServiceValidationFormat = jsonFormat2(ExecutionServiceValidation)

  implicit val ExecutionServiceOutputsFormat = jsonFormat2(ExecutionServiceOutputs)

  implicit val ExecutionServiceLogsFormat = jsonFormat2(ExecutionServiceLogs)

  implicit val ExecutionServiceWorkflowOptionsFormat = jsonFormat5(ExecutionServiceWorkflowOptions)

  implicit val TaskOutputFormat = jsonFormat2(TaskOutput)

  implicit val WorkflowOutputsFormat = jsonFormat2(WorkflowOutputs)

  implicit val SubmissionValidationInputFormat = jsonFormat2(SubmissionValidationInput)

  implicit val SubmissionValidationHeaderFormat = jsonFormat2(SubmissionValidationHeader)

  implicit val SubmissionValidationValueFormat = jsonFormat3(SubmissionValidationValue)

  implicit val SubmissionValidationEntityInputsFormat = jsonFormat2(SubmissionValidationEntityInputs)

  implicit val SubmissionValidationReportFormat = jsonFormat4(SubmissionValidationReport)

  implicit val WorkflowFormat = jsonFormat6(Workflow)

  implicit val WorkflowFailureFormat = jsonFormat4(WorkflowFailure)

  implicit val SubmissionFormat = jsonFormat9(Submission)

  implicit val WorkflowReportFormat = jsonFormat5(WorkflowReport)

  implicit val SubmissionReportFormat = jsonFormat8(SubmissionReport)

  implicit val SubmissionStatusResponseFormat = jsonFormat9(SubmissionStatusResponse)

  implicit val CallMetadataFormat = jsonFormat12(CallMetadata)

  implicit val ExecutionMetadataFormat = jsonFormat8(ExecutionMetadata)

  implicit val ActiveSubmissionFormat = jsonFormat3(ActiveSubmission)
}

trait RawlsEnumeration[T <: RawlsEnumeration[T]] { self: T =>
  def toString: String
  def withName(name:String): T
}

object WorkflowStatuses {
  val terminalStatuses: Seq[WorkflowStatus] = Seq(Failed, Succeeded, Aborted, Unknown)

  sealed trait WorkflowStatus extends RawlsEnumeration[WorkflowStatus] {
    def isDone = {
      terminalStatuses.contains(this)
    }
    override def toString = getClass.getSimpleName.stripSuffix("$")
    override def withName(name: String) = WorkflowStatuses.withName(name)
  }

  def withName(name: String): WorkflowStatus = {
    name match {
      case "Submitted" => Submitted
      case "Running" => Running
      case "Failed" => Failed
      case "Succeeded" => Succeeded
      case "Aborted" => Aborted
      case "Unknown" => Unknown
      case _ => throw new RawlsException(s"invalid WorkflowStatus [${name}]")
    }
  }

  case object Submitted extends WorkflowStatus
  case object Running extends WorkflowStatus
  case object Failed extends WorkflowStatus
  case object Succeeded extends WorkflowStatus
  case object Aborted extends WorkflowStatus
  case object Unknown extends WorkflowStatus
}


object SubmissionStatuses {
  sealed trait SubmissionStatus extends RawlsEnumeration[SubmissionStatus] {
    def isDone = { this == Done }
    override def toString = getClass.getSimpleName.stripSuffix("$")
    override def withName(name: String) = SubmissionStatuses.withName(name)
  }

  def withName(name: String): SubmissionStatus = {
    name match {
      case "Submitted" => Submitted
      case "Done" => Done
      case _ => throw new RawlsException(s"invalid SubmissionStatus [${name}]")
    }
  }

  case object Submitted extends SubmissionStatus
  case object Done extends SubmissionStatus
}

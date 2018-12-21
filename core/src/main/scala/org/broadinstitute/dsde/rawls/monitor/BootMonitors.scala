package org.broadinstitute.dsde.rawls.monitor

import akka.actor.{ActorRef, ActorSystem}
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import org.broadinstitute.dsde.rawls.dataaccess.{GoogleServicesDAO, SlickDataSource, _}
import org.broadinstitute.dsde.rawls.google.GooglePubSubDAO
import org.broadinstitute.dsde.rawls.jobexec.{SubmissionSupervisor, WorkflowSubmissionActor}
import org.broadinstitute.dsde.rawls.model.{UserInfo, WorkflowStatuses}
import org.broadinstitute.dsde.rawls.user.UserService
import org.broadinstitute.dsde.rawls.util
import org.broadinstitute.dsde.workbench.sam.google.BillingProjectMonitorSupervisor
import spray.json._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

// handles monitors which need to be started at boot time
object BootMonitors extends LazyLogging {

  def bootMonitors(system: ActorSystem, conf: Config, slickDataSource: SlickDataSource, gcsDAO: GoogleServicesDAO,
                   samDAO: SamDAO, pubSubDAO: GooglePubSubDAO, methodRepoDAO: MethodRepoDAO, dosResolver: DosResolver,
                   shardedExecutionServiceCluster: ExecutionServiceCluster, maxActiveWorkflowsTotal: Int,
                   maxActiveWorkflowsPerUser: Int, userServiceConstructor: (UserInfo) => UserService,
                   projectTemplate: ProjectTemplate, metricsPrefix: String, requesterPaysRole: String): Unit = {
    //Reset "Launching" workflows to "Queued"
    resetLaunchingWorkflows(slickDataSource)

    //Boot billing project creation monitor
    startCreatingBillingProjectMonitor(system, slickDataSource, pubSubDAO)

    //Boot submission monitor supervisor
    val submissionMonitorConfig = conf.getConfig("submissionmonitor")
    startSubmissionMonitorSupervisor(system, submissionMonitorConfig, slickDataSource, samDAO, gcsDAO, shardedExecutionServiceCluster, metricsPrefix)

    //Boot workflow submission actors
    startWorkflowSubmissionActors(system, conf, slickDataSource, gcsDAO, samDAO, methodRepoDAO, dosResolver, shardedExecutionServiceCluster, maxActiveWorkflowsTotal, maxActiveWorkflowsPerUser, metricsPrefix, requesterPaysRole)

    //Boot bucket deletion monitor
    startBucketDeletionMonitor(system, slickDataSource, gcsDAO)
  }

  private def startCreatingBillingProjectMonitor(system: ActorSystem, slickDataSource: SlickDataSource, pubSubDAO: GooglePubSubDAO): Unit = {
    system.actorOf(BillingProjectMonitorSupervisor.props(30 seconds, 10 seconds, pubSubDAO, "create_project_events", "create_project_sub", 1, slickDataSource))
  }

  private def startSubmissionMonitorSupervisor(system: ActorSystem, submissionMonitorConfig: Config, slickDataSource: SlickDataSource, samDAO: SamDAO, gcsDAO: GoogleServicesDAO, shardedExecutionServiceCluster: ExecutionServiceCluster, metricsPrefix: String) = {
    system.actorOf(SubmissionSupervisor.props(
      shardedExecutionServiceCluster,
      slickDataSource,
      samDAO,
      gcsDAO,
      gcsDAO.getBucketServiceAccountCredential,
      util.toScalaDuration(submissionMonitorConfig.getDuration("submissionPollInterval")),
      submissionMonitorConfig.getBoolean("trackDetailedSubmissionMetrics"),
      workbenchMetricBaseName = metricsPrefix
    ), "rawls-submission-supervisor")
  }

  private def startWorkflowSubmissionActors(system: ActorSystem, conf: Config, slickDataSource: SlickDataSource, gcsDAO: GoogleServicesDAO, samDAO: SamDAO, methodRepoDAO: MethodRepoDAO, dosResolver: DosResolver, shardedExecutionServiceCluster: ExecutionServiceCluster, maxActiveWorkflowsTotal: Int, maxActiveWorkflowsPerUser: Int, metricsPrefix: String, requesterPaysRole: String) = {
    for(i <- 0 until conf.getInt("executionservice.parallelSubmitters")) {
      system.actorOf(WorkflowSubmissionActor.props(
        slickDataSource,
        methodRepoDAO,
        gcsDAO,
        samDAO,
        dosResolver,
        shardedExecutionServiceCluster,
        conf.getInt("executionservice.batchSize"),
        gcsDAO.getBucketServiceAccountCredential,
        util.toScalaDuration(conf.getDuration("executionservice.processInterval")),
        util.toScalaDuration(conf.getDuration("executionservice.pollInterval")),
        maxActiveWorkflowsTotal,
        maxActiveWorkflowsPerUser,
        Try(conf.getObject("executionservice.defaultRuntimeOptions").render(ConfigRenderOptions.concise()).parseJson).toOption,
        conf.getBoolean("submissionmonitor.trackDetailedSubmissionMetrics"),
        metricsPrefix,
        requesterPaysRole
      ))
    }
  }

  private def startBucketDeletionMonitor(system: ActorSystem, slickDataSource: SlickDataSource, gcsDAO: GoogleServicesDAO) = {
    system.actorOf(BucketDeletionMonitor.props(slickDataSource, gcsDAO, 10 seconds, 6 hours))
  }

  private def resetLaunchingWorkflows(dataSource: SlickDataSource) = {
    Await.result(dataSource.inTransaction { dataAccess =>
      dataAccess.workflowQuery.batchUpdateStatus(WorkflowStatuses.Launching, WorkflowStatuses.Queued)
    }, 10 seconds)
  }
}
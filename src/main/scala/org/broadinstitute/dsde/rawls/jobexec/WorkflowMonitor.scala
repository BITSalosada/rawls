package org.broadinstitute.dsde.rawls.jobexec

import akka.actor.{ActorRef, Props, ReceiveTimeout, Actor}
import org.broadinstitute.dsde.rawls.RawlsException
import org.broadinstitute.dsde.rawls.dataaccess.{RawlsTransaction, ExecutionServiceDAO, WorkflowDAO, DataSource}
import org.broadinstitute.dsde.rawls.jobexec.WorkflowMonitor.StatusResponse
import org.broadinstitute.dsde.rawls.model.{ExecutionServiceStatus, WorkflowStatuses, Workflow}
import spray.http.HttpCookie

import scala.concurrent.duration.Duration
import scala.util.Try

/**
 * Created by dvoet on 6/29/15.
 */
object WorkflowMonitor {
  def props(pollInterval: Duration, executionServiceDAO: ExecutionServiceDAO, workflowDAO: WorkflowDAO, datasource: DataSource, authCookie: HttpCookie)(parent: ActorRef, workflow: Workflow): Props = {
    Props(new WorkflowMonitor(parent, pollInterval, workflow, executionServiceDAO, workflowDAO, datasource, authCookie))
  }

  sealed trait WorkflowMonitorMessage
  case class StatusResponse(statusResponse: Try[ExecutionServiceStatus]) extends WorkflowMonitorMessage
}

/**
 * Polls executionServiceDAO every pollInterval to get workflow status and reports changes via a SubmissionMonitor.WorkflowStatusChange
 * message to parent.
 * @param parent actor ref to report changes to
 * @param pollInterval
 * @param workflow
 * @param executionServiceDAO
 * @param workflowDAO
 * @param datasource
 * @param authCookie auth cookie required by executionServiceDAO
 */
class WorkflowMonitor(parent: ActorRef, pollInterval: Duration, workflow: Workflow, executionServiceDAO: ExecutionServiceDAO, workflowDAO: WorkflowDAO, datasource: DataSource, authCookie: HttpCookie) extends Actor {
  import context._

  setReceiveTimeout(pollInterval)

  override def receive = {
    case ReceiveTimeout => checkWorkflowStatus()
    case StatusResponse(statusResponse) => updateStatus(statusResponse)
  }

  def checkWorkflowStatus(): Unit = datasource inTransaction { txn =>
    system.log.debug("polling execution service for workflow {}", workflow.id)
    executionServiceDAO.status(workflow.id, authCookie).onComplete { status =>
      self ! StatusResponse(status)
    }
  }

  def updateStatus(statusResponse: Try[ExecutionServiceStatus]) = datasource inTransaction { txn =>
    val status = WorkflowStatuses.withName(statusResponse.get.status)

    val refreshedWorkflow = workflowDAO.get(workflow.workspaceNamespace, workflow.workspaceName, workflow.id, txn).getOrElse(
      throw new RawlsException(s"workflow ${workflow} could not be found")
    )
    if (refreshedWorkflow.status != status) {
      val updatedWorkflow: Workflow = refreshedWorkflow.copy(status = status)
      onWorkflowDone(updatedWorkflow, txn)
      parent ! SubmissionMonitor.WorkflowStatusChange(updatedWorkflow)
    }

    if (status.isDone) {
      stop(self)
    }
  }

  def onWorkflowDone(updateWorkflow: Workflow, txn: RawlsTransaction): Unit = {
    // call any code to update entity model here
  }
}

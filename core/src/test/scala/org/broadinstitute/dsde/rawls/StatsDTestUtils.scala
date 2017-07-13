package org.broadinstitute.dsde.rawls

import java.util.concurrent.TimeUnit

import com.codahale.metrics.{MetricFilter, SharedMetricRegistries}
import com.readytalk.metrics.{StatsD, StatsDReporter}
import com.typesafe.scalalogging.LazyLogging
import org.broadinstitute.dsde.rawls.model.SubmissionStatuses.SubmissionStatus
import org.broadinstitute.dsde.rawls.model.WorkflowStatuses.WorkflowStatus
import org.broadinstitute.dsde.rawls.model.{Submission, Workspace, WorkspaceName}
import org.mockito.Mockito.{atLeast => mokitoAtLeast, inOrder => mockitoInOrder}
import org.scalatest.concurrent.Eventually
import org.scalatest.mock.MockitoSugar

import scala.collection.JavaConverters._
import scala.concurrent.duration._

/**
  * Created by rtitle on 6/29/17.
  */
trait StatsDTestUtils { this: MockitoSugar with Eventually with LazyLogging with RawlsTestUtils =>

  protected def withStatsD[T](testCode: => T)(verify: Seq[(String, String)] => Unit = _ => ()): T = {
    val statsD = mock[StatsD]
    SharedMetricRegistries.getOrCreate("default").removeMatching(MetricFilter.ALL)
    val reporter = StatsDReporter.forRegistry(SharedMetricRegistries.getOrCreate("default"))
      .convertRatesTo(TimeUnit.SECONDS)
      .convertDurationsTo(TimeUnit.MILLISECONDS)
      .build(statsD)
    reporter.start(1, TimeUnit.SECONDS)
    try {
      val result = testCode
      eventually(timeout(50 seconds)) {
        val order = mockitoInOrder(statsD)
        order.verify(statsD).connect()
        val metricCaptor = captor[String]
        val valueCaptor = captor[String]
        order.verify(statsD, mokitoAtLeast(1)).send(metricCaptor.capture, valueCaptor.capture)
        order.verify(statsD).close()
        verify(metricCaptor.getAllValues.asScala.zip(valueCaptor.getAllValues.asScala))
      }
      result
    } finally {
      reporter.stop()
      SharedMetricRegistries.getOrCreate("default").removeMatching(MetricFilter.ALL)
    }
  }

  protected def expectedWorkflowStatusMetric(workspace: Workspace, submission: Submission, workflowStatus: WorkflowStatus, name: Option[String], expectedTimes: Int): (String, String) =
    (s"test.workspace.${workspace.toWorkspaceName.toString.replace('/', '.')}.submission.${submission.submissionId}.workflowStatus.${workflowStatus.toString}${name.map(n => s".$n").getOrElse("")}", expectedTimes.toString)

  protected def expectedWorkflowStatusMetric(workspace: Workspace, submission: Submission, workflowStatus: WorkflowStatus, name: Option[String] = None): (String, String) =
    expectedWorkflowStatusMetric(workspace, submission, workflowStatus, name, submission.workflows.size)

  protected def expectedSubmissionStatusMetric(workspace: Workspace, submissionStatus: SubmissionStatus, expectedTimes: Int = 1): (String, String) =
    expectedSubmissionStatusMetric(workspace.toWorkspaceName, submissionStatus, expectedTimes)

  protected def expectedSubmissionStatusMetric(workspaceName: WorkspaceName, submissionStatus: SubmissionStatus, expectedTimes: Int): (String, String) =
    (s"test.workspace.${workspaceName.toString.replace('/', '.')}.submissionStatus.${submissionStatus.toString}", expectedTimes.toString)

  protected def expectedAPICounterMetric(workspaceName: WorkspaceName, submissionId: String, expectedTimes: Int): (String, String) =
    (s"rawls.rawls.request.workspaces_${workspaceName.toString.replace('/', '_')}_submissions_${submissionId}", expectedTimes.toString)

  //protected def expectedAPITimerRateMetric(api,workspaceName: WorkspaceName, submissionId: String, )
}

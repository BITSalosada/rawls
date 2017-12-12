package org.broadinstitute.dsde.rawls.dataaccess

import akka.actor.ActorSystem
import com.google.api.services.bigquery.model.{TableCell, TableRow}
import org.broadinstitute.dsde.rawls.RawlsTestUtils
import org.broadinstitute.dsde.workbench.google.mock.MockGoogleBigQueryDAO
import org.scalatest.FlatSpec

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class SubmissionCostServiceSpec extends FlatSpec with RawlsTestUtils {
  implicit val actorSystem = ActorSystem("SubmissionCostServiceSpec")
  val bigQueryDAO = new MockGoogleBigQueryDAO
  val submissionCostService = SubmissionCostService.constructor("test", "test", bigQueryDAO)

  val rows = Future(List(
    new TableRow().setF(List(new TableCell().setV("wfKey"), new TableCell().setV("wf1"), new TableCell().setV(1.32f)).asJava),
    new TableRow().setF(List(new TableCell().setV("wfKey"), new TableCell().setV("wf2"), new TableCell().setV(3f)).asJava),
    new TableRow().setF(List(new TableCell().setV("wfKey"), new TableCell().setV("wf3"), new TableCell().setV(101.00f)).asJava)
  ).asJava)

  "SubmissionCostService" should "extract a map of workflow ID to cost" in {
    val expected = Map("wf1" -> 1.32f, "wf2" -> 3.00f, "wf3" -> 101.00f)
    assertResult(expected){
      Await.result(submissionCostService.extractWorkflowCostResults(rows), 1 minute)
    }
  }

}
package org.broadinstitute.dsde.rawls.trivial

import org.scalatest.FlatSpec
import spray.http.StatusCodes
import spray.routing.{HttpService, Directives, Route}
import spray.testkit.ScalatestRouteTest

trait TrivialDirectivesTextFixture extends Directives {

  def trivialRoute: Route =
    path("test_route") {
      get { requestContext =>
        println("get")

        requestContext.complete("trivial response")
       }
    }

}

class TrivialDirectivesSpec extends FlatSpec with TrivialDirectivesTextFixture with ScalatestRouteTest with HttpService {

  def actorRefFactory = system

  "TrivialDirectives" should "trivially match" in {
    Get("/test_route") ~> sealRoute(trivialRoute) ~> check {
      println("check")

      assertResult(StatusCodes.OK) {
        println("status " + status)
        status
      }

      assertResult("trivial response") {
        println("response " + response)
        responseAs[String]
      }
    }
  }
}

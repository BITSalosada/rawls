package org.broadinstitute.dsde.rawls.integrationtest

import java.util.UUID

import org.broadinstitute.dsde.rawls.model.WorkspaceJsonSupport._
import org.broadinstitute.dsde.rawls.model._
import org.broadinstitute.dsde.rawls.webservice.{GoogleAuthApiService, MethodConfigApiService, SubmissionApiService, WorkspaceApiService}
import spray.http.StatusCodes

import scala.concurrent.duration._

class RemoteServicesSpec extends IntegrationTestBase with WorkspaceApiService with MethodConfigApiService with SubmissionApiService with GoogleAuthApiService {
  def actorRefFactory = system

  // setup workspace service
  val workspaceServiceConstructor = workspaceServiceWithDbName("remote-services-latest")

  implicit val routeTestTimeout = RouteTestTimeout(5.seconds)

  val wsns = "namespace"
  val wsname = UUID.randomUUID().toString
  val methodConfig = MethodConfiguration(wsns, "testConfig", "samples", Map("ready" -> "true"), Map("param1" -> "foo"), Map("out" -> "bar"), WorkspaceName(wsns, wsname), MethodStoreConfiguration("method-a-config", "dsde", "1"), MethodStoreMethod("method-a", "dsde", "1"))
  val methodConfigName = MethodConfigurationName(methodConfig.name, methodConfig.namespace, methodConfig.workspaceName)

  val workspace = WorkspaceRequest(wsns, wsname, Map.empty)

  val uniqueMethodConfigName = UUID.randomUUID.toString
  val newMethodConfigName = MethodConfigurationName(uniqueMethodConfigName, methodConfig.namespace, methodConfig.workspaceName)
  val methodRepoGood = MethodRepoConfigurationQuery("rawls_integration_testing", "rawls_test_good", "2", newMethodConfigName)
  val methodRepoMissing = MethodRepoConfigurationQuery("rawls_integration_testing", "rawls_test_missing", "1", methodConfigName)
  val methodRepoEmptyPayload = MethodRepoConfigurationQuery("rawls_integration_testing", "rawls_test_empty_payload", "1", methodConfigName)
  val methodRepoBadPayload = MethodRepoConfigurationQuery("rawls_integration_testing", "rawls_test_bad_payload", "1", methodConfigName)

  val copyFromMethodRepoEndpoint = "/methodconfigs/copyFromMethodRepo"

  "RemoteServicesSpec" should "copy a method config from the method repo" ignore {
    // need to init workspace
    Post(s"/workspaces", httpJson(workspace)) ~>
      addOpenAmCookie ~>
      sealRoute(postWorkspaceRoute) ~>
      check {
        assertResult(StatusCodes.Created) {
          status
        }
      }

    Post(copyFromMethodRepoEndpoint, httpJson(methodRepoGood)) ~>
      addOpenAmCookie ~>
      sealRoute(copyMethodRepoConfigurationRoute) ~>
      check {
        assertResult(StatusCodes.Created) {
          status
        }
      }

    Post(copyFromMethodRepoEndpoint, httpJson(methodRepoGood)) ~>
      addOpenAmCookie ~>
      sealRoute(copyMethodRepoConfigurationRoute) ~>
      check {
        assertResult(StatusCodes.Conflict) {
          status
        }
      }

    Post(copyFromMethodRepoEndpoint, httpJson(methodRepoMissing)) ~>
      addOpenAmCookie ~>
      sealRoute(copyMethodRepoConfigurationRoute) ~>
      check {
        assertResult(StatusCodes.NotFound) {
          status
        }
      }

    Post(copyFromMethodRepoEndpoint, httpJson(methodRepoEmptyPayload)) ~>
      addOpenAmCookie ~>
      sealRoute(copyMethodRepoConfigurationRoute) ~>
      check {
        assertResult(StatusCodes.UnprocessableEntity) {
          status
        }
      }

    Post(copyFromMethodRepoEndpoint, httpJson(methodRepoBadPayload)) ~>
      addOpenAmCookie ~>
      sealRoute(copyMethodRepoConfigurationRoute) ~>
      check {
        assertResult(StatusCodes.UnprocessableEntity) {
          status
        }
      }

  }

}

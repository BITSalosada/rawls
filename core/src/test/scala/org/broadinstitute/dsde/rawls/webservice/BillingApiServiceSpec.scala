package org.broadinstitute.dsde.rawls.webservice

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.OAuth2BearerToken
import akka.http.scaladsl.server.Route.{seal => sealRoute}
import org.broadinstitute.dsde.rawls.dataaccess._
import org.broadinstitute.dsde.rawls.dataaccess.slick.{RawlsBillingProjectOperationRecord, RawlsBillingProjectRecord, ReadAction}
import org.broadinstitute.dsde.rawls.google.MockGooglePubSubDAO
import org.broadinstitute.dsde.rawls.model._
import org.broadinstitute.dsde.rawls.openam.MockUserInfoDirectives
import spray.json.DefaultJsonProtocol._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class BillingApiServiceSpec extends ApiServiceSpec {
  import org.broadinstitute.dsde.rawls.model.UserAuthJsonSupport._

  case class TestApiService(dataSource: SlickDataSource, gcsDAO: MockGoogleServicesDAO, gpsDAO: MockGooglePubSubDAO)(implicit override val executionContext: ExecutionContext) extends ApiServices with MockUserInfoDirectives

  def withApiServices[T](dataSource: SlickDataSource, gcsDAO: MockGoogleServicesDAO = new MockGoogleServicesDAO("test"))(testCode: TestApiService =>  T): T = {
    val apiService = new TestApiService(dataSource, gcsDAO, new MockGooglePubSubDAO)
    try {
      testCode(apiService)
    } finally {
      apiService.cleanupSupervisor
    }
  }
  
  def withCleanApiServices[T](testCode: TestApiService => T): T = {
    withCustomTestDatabase(emptyData) { dataSource: SlickDataSource =>
      withApiServices(dataSource)(testCode)
    }
  }

  def createProject(project: RawlsBillingProject, owner: RawlsUser = testData.userOwner): Unit = {
    val projectWithOwner = project.copy()

    runAndWait(rawlsBillingProjectQuery.create(projectWithOwner))
  }

  private def createTestBillingProject(services: TestApiService, project: RawlsBillingProject, ownerUserInfo: UserInfo) = {
    val policies = Map(
      SamBillingProjectPolicyNames.workspaceCreator -> SamPolicy(Set.empty, Set.empty, Set(SamProjectRoles.workspaceCreator)),
      SamBillingProjectPolicyNames.canComputeUser -> SamPolicy(Set.empty, Set.empty, Set(SamProjectRoles.batchComputeUser, SamProjectRoles.notebookUser)),
      SamBillingProjectPolicyNames.owner -> SamPolicy(Set(ownerUserInfo.userEmail.value), Set.empty, Set(SamProjectRoles.owner))
    )

    Await.result(services.samDAO.createResourceFull(SamResourceTypeNames.billingProject, project.projectName.value, policies, Set.empty, ownerUserInfo), Duration.Inf)
    runAndWait(rawlsBillingProjectQuery.create(project))
  }

  "BillingApiService" should "return 200 when adding a user to a billing project" in withCleanApiServices { services =>
    val project = billingProjectFromName("new_project")

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)
    Await.result(services.samDAO.registerUser(UserInfo(RawlsUserEmail("writer-access"), OAuth2BearerToken("token"), 123, RawlsUserSubjectId("123456789876543212346"))), Duration.Inf)

    createTestBillingProject(services, project, userInfo)

    Put(s"/billing/${project.projectName.value}/user/${testData.userWriter.userEmail.value}") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.OK) {
          status
        }
      }

    Put(s"/billing/${project.projectName.value}/owner/${testData.userWriter.userEmail.value}") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.OK) {
          status
        }
      }
  }

  it should "return 403 when adding a user to a non-owned billing project" in withCleanApiServices { services =>
    val project = billingProjectFromName("no_access")

    val readerUserInfo = UserInfo(RawlsUserEmail("reader-access"), OAuth2BearerToken("token"), 123, RawlsUserSubjectId("123456789876543212347"))

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)
    Await.result(services.samDAO.registerUser(readerUserInfo), Duration.Inf)

    createTestBillingProject(services, project, readerUserInfo)

    withStatsD {
      Put(s"/billing/${project.projectName.value}/user/${testData.userWriter.userEmail.value}") ~> services.sealedInstrumentedRoutes ~>
        check {
          assertResult(StatusCodes.Forbidden) {
            status
          }
        }

      Put(s"/billing/${project.projectName.value}/owner/${testData.userWriter.userEmail.value}") ~> services.sealedInstrumentedRoutes ~>
        check {
          assertResult(StatusCodes.Forbidden) {
            status
          }
        }
    } { capturedMetrics =>
      val expected = expectedHttpRequestMetrics("put", s"billing.redacted.redacted.redacted", StatusCodes.Forbidden.intValue, 2)
      assertSubsetOf(expected, capturedMetrics)
    }
  }

  it should "return 404 when adding a nonexistent user to a billing project" in withCleanApiServices { services =>
    val project = billingProjectFromName("no_access")

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)

    createTestBillingProject(services, project, userInfo)

    Put(s"/billing/${project.projectName.value}/nobody") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.NotFound) {
          status
        }
      }
  }

  it should "return 403 when adding a user to a nonexistent project" in withCleanApiServices { services =>
    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)

    Put(s"/billing/missing_project/user/${testData.userOwner.userEmail.value}") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.NotFound) { //API_CHANGE: this was previously a Forbidden. Why?
          status
        }
      }
  }

  it should "return 200 when removing a user from a billing project" in withCleanApiServices { services =>
    val project = billingProjectFromName("new_project")

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)
    Await.result(services.samDAO.registerUser(UserInfo(RawlsUserEmail("writer-access"), OAuth2BearerToken("token"), 123, RawlsUserSubjectId("123456789876543212346"))), Duration.Inf)

    withStatsD {
      val createRequest = CreateRawlsBillingProjectFullRequest(project.projectName, services.gcsDAO.accessibleBillingAccountName)

      import UserAuthJsonSupport.CreateRawlsBillingProjectFullRequestFormat

      Post(s"/billing", httpJson(createRequest)) ~>
        sealRoute(services.billingRoutes) ~>
        check {
          assertResult(StatusCodes.Created) {
            status
          }
        }

      Put(s"/billing/${project.projectName.value}/user/${testData.userWriter.userEmail.value}") ~> services.sealedInstrumentedRoutes ~>
        check {
          assertResult(StatusCodes.OK) {
            status
          }
        }

      Delete(s"/billing/${project.projectName.value}/user/${testData.userWriter.userEmail.value}") ~> services.sealedInstrumentedRoutes ~>
        check {
          assertResult(StatusCodes.OK) {
            status
          }
        }
    } { capturedMetrics =>
      val expected = expectedHttpRequestMetrics("put", s"billing.redacted.redacted.redacted", StatusCodes.OK.intValue, 1) ++
        expectedHttpRequestMetrics("delete", s"billing.redacted.redacted.redacted", StatusCodes.OK.intValue, 1)
      assertSubsetOf(expected, capturedMetrics)
    }
  }

  it should "return 403 when removing a user from a non-owned billing project" in withCleanApiServices { services =>
    val project = billingProjectFromName("no_access")

    val writerUserInfo = UserInfo(RawlsUserEmail("writer-access"), OAuth2BearerToken("token"), 123, RawlsUserSubjectId("123456789876543212346"))

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)
    Await.result(services.samDAO.registerUser(writerUserInfo), Duration.Inf)

    createTestBillingProject(services, project, writerUserInfo)

    Delete(s"/billing/${project.projectName.value}/owner/${testData.userWriter.userEmail.value}") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.Forbidden) {
          status
        }
      }
  }

  it should "return 404 when removing a nonexistent user from a billing project" in withCleanApiServices { services =>
    val project = billingProjectFromName("test_good")

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)

    createTestBillingProject(services, project, userInfo)

    Delete(s"/billing/${project.projectName.value}/user/nobody") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.NotFound) {
          status
        }
      }
  }

  it should "return 403 when removing a user from a nonexistent billing project" in withCleanApiServices { services =>

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)

    Delete(s"/billing/missing_project/user/${testData.userOwner.userEmail.value}") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.NotFound) { //API_CHANGE: this was previously forbidden
          status
        }
      }
  }

  it should "return 204 when creating a project with accessible billing account" in withCleanApiServices { services =>
    val projectName = RawlsBillingProjectName("test_good")

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)

    Post("/billing", CreateRawlsBillingProjectFullRequest(projectName, services.gcsDAO.accessibleBillingAccountName)) ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.Created) {
          status
        }

        import driver.api._
        val query: ReadAction[Seq[RawlsBillingProjectRecord]] = rawlsBillingProjectQuery.filter(_.projectName === projectName.value).result
        val projects: Seq[RawlsBillingProjectRecord] = runAndWait(query)
        projects match {
          case Seq() => fail("project does not exist in db")
          case Seq(project) =>
            assertResult("gs://" + services.gcsDAO.getCromwellAuthBucketName(projectName)) {
              project.cromwellAuthBucketUrl
            }
          case _ => fail("too many projects")
        }
      }
  }

  ignore should "rollback billing project inserts when there is a google error" in withCleanApiServices { services => //dataSource: SlickDataSource =>
//    withApiServices(dataSource, new MockGoogleServicesDAO("test") {
//      override def createProject(projectName: RawlsBillingProjectName, billingAccount: RawlsBillingAccount): Future[RawlsBillingProjectOperationRecord] = {
//        Future.failed(new Exception("test exception"))
//      }
//    }) { services =>
//      val projectName = RawlsBillingProjectName("test_good2")
//
//      Post("/billing", CreateRawlsBillingProjectFullRequest(projectName, services.gcsDAO.accessibleBillingAccountName)) ~>
//        sealRoute(services.billingRoutes) ~>
//        check {
//          assertResult(StatusCodes.InternalServerError) {
//            status
//          }
//          runAndWait(rawlsBillingProjectQuery.load(projectName)) map { p => fail("did not rollback project inserts: " + p) }
//        }
//    }
    assert(false)
  }

  it should "return 400 when creating a project with inaccessible to firecloud billing account" in withCleanApiServices { services =>
    Post("/billing", CreateRawlsBillingProjectFullRequest(RawlsBillingProjectName("test_bad1"), services.gcsDAO.inaccessibleBillingAccountName)) ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.BadRequest) {
          status
        }
      }
  }

  it should "return 403 when creating a project with inaccessible to user billing account" in withCleanApiServices { services =>
    Post("/billing", CreateRawlsBillingProjectFullRequest(RawlsBillingProjectName("test_bad1"), RawlsBillingAccountName("this does not exist"))) ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.Forbidden) {
          status
        }
      }
  }

  it should "return 200 when listing billing project members as owner" in withCleanApiServices { services =>
    val project = billingProjectFromName("test_good")

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)

    createTestBillingProject(services, project, userInfo)

    Get(s"/billing/${project.projectName.value}/members") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.OK) {
          status
        }
        assertResult(Seq(RawlsBillingProjectMember(testData.userOwner.userEmail, ProjectRoles.Owner))) {
          responseAs[Seq[RawlsBillingProjectMember]]
        }
      }
  }

  it should "return 403 when listing billing project members as non-owner" in withCleanApiServices { services =>
    val project = billingProjectFromName("no_access")

    val projectOwnerUserInfo = userInfo.copy(userEmail = RawlsUserEmail("project-owner"), userSubjectId = RawlsUserSubjectId("11111111"))

    Await.result(services.samDAO.registerUser(userInfo), Duration.Inf)
    Await.result(services.samDAO.registerUser(projectOwnerUserInfo), Duration.Inf)

    createTestBillingProject(services, project, projectOwnerUserInfo)

    Get(s"/billing/${project.projectName.value}/members") ~>
      sealRoute(services.billingRoutes) ~>
      check {
        assertResult(StatusCodes.Forbidden) {
          status
        }
      }
  }
}

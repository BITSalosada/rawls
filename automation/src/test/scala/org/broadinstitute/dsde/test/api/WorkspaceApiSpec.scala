package org.broadinstitute.dsde.test.api

import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.broadinstitute.dsde.workbench.config.{Credentials, UserPool}
import org.broadinstitute.dsde.workbench.auth.AuthToken
import org.broadinstitute.dsde.workbench.fixture._
import org.broadinstitute.dsde.workbench.service._
import org.broadinstitute.dsde.workbench.util.Retry
import org.broadinstitute.dsde.workbench.service.test.{CleanUp, RandomUtil}
import org.broadinstitute.dsde.rawls.model._
import org.broadinstitute.dsde.rawls.model.WorkspaceJsonSupport._
import org.broadinstitute.dsde.rawls.model.Attributable.AttributeMap
import org.broadinstitute.dsde.rawls.model.AttributeUpdateOperations._
import org.scalatest.{FreeSpecLike, Matchers}
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Minutes, Seconds, Span}

import spray.json._
import DefaultJsonProtocol._

class WorkspaceApiSpec extends TestKit(ActorSystem("MySpec")) with FreeSpecLike with Matchers with Eventually
  with CleanUp with RandomUtil with Retry
  with BillingFixtures with WorkspaceFixtures with MethodFixtures {

  val Seq(studentA, studentB) = UserPool.chooseStudents(2)
  val studentAToken: AuthToken = studentA.makeAuthToken()
  val studentBToken: AuthToken = studentB.makeAuthToken()

  val owner: Credentials = UserPool.chooseProjectOwner
  val ownerAuthToken: AuthToken = owner.makeAuthToken()

  val operations = Array(Map("op" -> "AddUpdateAttribute", "attributeName" -> "participant1", "addUpdateAttribute" -> "testparticipant"))
  val entity: Array[Map[String, Any]] = Array(Map("name" -> "participant1", "entityType" -> "participant", "operations" -> operations))

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(timeout = scaled(Span(1, Minutes)), interval = scaled(Span(20, Seconds)))

  "Rawls" - {
    "should allow project owners" - {
      "to create, clone, and delete workspaces" in {
        implicit val token: AuthToken = ownerAuthToken

        withCleanBillingProject(owner) { projectName =>
          val workspaceName = prependUUID("owner-test-workspace")
          val workspaceCloneName = s"$workspaceName-copy"

          Rawls.workspaces.create(projectName, workspaceName)
          workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)).workspace.name should be(workspaceName)

          Rawls.workspaces.clone(projectName, workspaceName, projectName, workspaceCloneName)
          workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceCloneName)).workspace.name should be(workspaceCloneName)

          Rawls.workspaces.delete(projectName, workspaceName)
          assertNoAccessToWorkspace(projectName, workspaceName)

          Rawls.workspaces.delete(projectName, workspaceCloneName)
          assertNoAccessToWorkspace(projectName, workspaceCloneName)
        }
      }

      "to add readers with can-share access" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("share-reader")) { workspaceName =>
            // grant reader access and can-share permission to student A
            val shareWithStudentA: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Reader, Some(true), Some(false)))
            Rawls.workspaces.updateAcl(projectName, workspaceName, shareWithStudentA)(ownerAuthToken)

            eventually {
              val studentAWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              studentAWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Read)
              studentAWorkspaceDetails.canShare should be(true)
            }

            // check that student A can share the workspace with student B
            val shareWithStudentB: Set[AclEntry] = Set(AclEntry(studentB.email, WorkspaceAccessLevel.Reader, Some(false), Some(false)))
            Rawls.workspaces.updateAcl(projectName, workspaceName, shareWithStudentB)(studentAToken)

            eventually {
              val studentBWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentBToken))
              studentBWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Read)
            }
          }(ownerAuthToken)
        }
      }

      "to add readers without can-share access" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("no-share-reader")) { workspaceName =>
            // grant reader access to student A
            val shareWithStudentA: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Reader, Some(false), Some(false)))
            Rawls.workspaces.updateAcl(projectName, workspaceName, shareWithStudentA)(ownerAuthToken)

            eventually {
              val studentAWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              studentAWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Read)
              studentAWorkspaceDetails.canShare should be(false)
            }

            // check that student A cannot share the workspace with student B
            val shareWithStudentB: Set[AclEntry] = Set(AclEntry(studentB.email, WorkspaceAccessLevel.Reader, Some(false), Some(false)))
            assertExceptionStatusCode(intercept[RestException](Rawls.workspaces.updateAcl(projectName, workspaceName, shareWithStudentB)(studentAToken)), 403)
            assertNoAccessToWorkspace(projectName, workspaceName)(studentBToken)
          }(ownerAuthToken)
        }
      }

      "to add writers with can-compute access and then revoke can-compute" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("compute-writer")) { workspaceName =>
            val writerCanCompute: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Writer, Some(false), Some(true)))
            Rawls.workspaces.updateAcl(projectName, workspaceName, writerCanCompute)(ownerAuthToken)

            eventually {
              val canComputeWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              canComputeWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Write)
              canComputeWorkspaceDetails.canCompute should be(true)
            }

            val revokeCanCompute: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Writer, Some(false), Some(false)))
            Rawls.workspaces.updateAcl(projectName, workspaceName, revokeCanCompute)(ownerAuthToken)

            eventually {
              val noComputeWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              noComputeWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Write)
              noComputeWorkspaceDetails.canCompute should be(false)
            }
          }(ownerAuthToken)
        }
      }

      "to change a writer's access level to reader" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("reader-to-writer")) { workspaceName =>
            val writerAccess: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Writer))
            Rawls.workspaces.updateAcl(projectName, workspaceName, writerAccess)(ownerAuthToken)

            eventually {
              val writerWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              writerWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Write)
              writerWorkspaceDetails.canCompute should be(true)
            }

            val readerAccess: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Reader))
            Rawls.workspaces.updateAcl(projectName, workspaceName, readerAccess)(ownerAuthToken)

            eventually {
              val readerWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              readerWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Read)
              readerWorkspaceDetails.canCompute should be(false)
            }
          }(ownerAuthToken)
        }
      }

      "to change a writer's access level to no access" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("revoke-writer")) { workspaceName =>
            val writerAccess: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Writer))
            Rawls.workspaces.updateAcl(projectName, workspaceName, writerAccess)(ownerAuthToken)

            eventually {
              val writerWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              writerWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Write)
            }

            val noAccess: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.NoAccess))
            Rawls.workspaces.updateAcl(projectName, workspaceName, noAccess)(ownerAuthToken)

            assertNoAccessToWorkspace(projectName, workspaceName)(studentAToken)
          }(ownerAuthToken)
        }
      }

      "to add an owner to a workspace, but not to change the new owner's permissions" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("two-owners")) { workspaceName =>
            val ownerAccess: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Owner))
            Rawls.workspaces.updateAcl(projectName, workspaceName, ownerAccess)(ownerAuthToken)

            eventually {
              val secondOwnerWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              secondOwnerWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Owner)
              secondOwnerWorkspaceDetails.canCompute should be(true)
              secondOwnerWorkspaceDetails.canShare should be(true)
            }

            // try removing new owner's sharing and compute abilities, it should not work
            val changePermissions: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Owner, Some(false), Some(false)))
            Rawls.workspaces.updateAcl(projectName, workspaceName, changePermissions)(ownerAuthToken)

            eventually {
              val newOwnerWorkspaceDetails = workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(studentAToken))
              newOwnerWorkspaceDetails.accessLevel should equal(WorkspaceAccessLevels.Owner)
              newOwnerWorkspaceDetails.canCompute should be(true)
              newOwnerWorkspaceDetails.canShare should be(true)
            }
          }(ownerAuthToken)
        }
      }

      val testAttributes: Map[String, String] = Map("A-key" -> "A-value", "B-key" -> "B-value", "C-key" -> "C-value")
      val testAttributeNamespace = "default"
      val attributeMap: AttributeMap = testAttributes.map(keyValuePairs => AttributeName(testAttributeNamespace, keyValuePairs._1) -> AttributeString(keyValuePairs._2))

      "to add workspace attributes" in {
        implicit val token: AuthToken = ownerAuthToken
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("add-attributes")) { workspaceName =>
            val attributeUpdates = attributeMap.map(attrTuple => AddUpdateAttribute(attrTuple._1, attrTuple._2)).toList

            Rawls.workspaces.updateAttributes(projectName, workspaceName, attributeUpdates)
            eventually {
              workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)).workspace.attributes should be (attributeMap)
            }
          }
        }
      }

      "to remove workspace attributes" in {
        implicit val token: AuthToken = ownerAuthToken
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("delete-attributes"), attributes = Some(testAttributes)) { workspaceName =>
            val attributeNameToRemove = testAttributes.keys.head
            val attributeUpdates = List(RemoveAttribute(AttributeName(testAttributeNamespace, attributeNameToRemove)))

            Rawls.workspaces.updateAttributes(projectName, workspaceName, attributeUpdates)
            eventually {
              workspaceResponse(Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)).workspace.attributes should be (attributeMap.filter(attribute => attribute._1.name != attributeNameToRemove))
            }
          }
        }
      }
    }

    "should not allow project owners" - {
      "to grant readers can-compute access" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("no-compute-reader")) { workspaceName =>
            val computeReader: Set[AclEntry] = Set(AclEntry(studentA.email, WorkspaceAccessLevel.Reader, Some(false), Some(true)))

            val exception = intercept[RestException](Rawls.workspaces.updateAcl(projectName, workspaceName, computeReader)(ownerAuthToken))

            exception.message.parseJson.asJsObject.fields("message").convertTo[String] should equal("may not grant readers compute access")
          }(ownerAuthToken)
        }
      }
    }

    "should not allow readers" - {
      def getMethodConfig(method: Method, projectName: String, destWorkspaceName: String): Map[String, Any] = Map(
        "name" -> s"destination-${method.methodName}",
        "namespace" -> s"destination-${method.methodNamespace}",
        "workspaceName" -> Map(
          "namespace" -> projectName,
          "name" -> destWorkspaceName)
      )

      "to import method configs from another workspace" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("reader-import-config-dest-workspace"), aclEntries = List(AclEntry(studentA.email, WorkspaceAccessLevel.Reader))) { destWorkspaceName =>
            withWorkspace(projectName, prependUUID("method-config-source-workspace"), aclEntries = List(AclEntry(studentA.email, WorkspaceAccessLevel.Reader))) { sourceWorkspaceName =>
              withMethod("reader-import-from-workspace", MethodData.SimpleMethod) { methodName =>
                val method = MethodData.SimpleMethod.copy(methodName = methodName)

                val destMethodConfig = getMethodConfig(method, projectName, destWorkspaceName)
                val sourceMethodConfig = getMethodConfig(method, projectName, sourceWorkspaceName)

                Rawls.methodConfigs.createMethodConfigInWorkspace(
                  projectName, sourceWorkspaceName, method, method.methodNamespace, method.methodName, 1,
                  Map.empty, Map.empty, method.rootEntityType)(ownerAuthToken)

                eventually {
                  val copyFromWorkspaceException = intercept[RestException] {
                    Rawls.methodConfigs.copyMethodConfigFromWorkspace(sourceMethodConfig, destMethodConfig)(studentAToken)
                  }
                  assertExceptionStatusCode(copyFromWorkspaceException, 403)
                }
              }(ownerAuthToken)
            }(ownerAuthToken)
          }(ownerAuthToken)
        }
      }

      "to import method configs from the method repo" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("reader-import-config-dest-workspace"), aclEntries = List(AclEntry(studentA.email, WorkspaceAccessLevel.Reader))) { destWorkspaceName =>
            withMethod("reader-import-from-method-repo", MethodData.SimpleMethod) { methodName =>
              val method = MethodData.SimpleMethod.copy(methodName = methodName)

              val methodRepoConfig = Map(
                "methodRepoNamespace" -> SimpleMethodConfig.configNamespace,
                "methodRepoName" -> SimpleMethodConfig.configName,
                "methodRepoSnapshotId" -> SimpleMethodConfig.snapshotId,
                "destination" -> getMethodConfig(method, projectName, destWorkspaceName)
              )

              // studentA needs permission to access the method config or importing from method repo will return 404 not 403
              Orchestration.methodConfigurations.setMethodConfigPermission(
                SimpleMethodConfig.configNamespace,
                SimpleMethodConfig.configName,
                SimpleMethodConfig.snapshotId,
                studentA.email,
                "OWNER"
              )(ownerAuthToken)

              eventually {
                val copyFromMethodRepoException = intercept[RestException] {
                  Rawls.methodConfigs.copyMethodConfigFromMethodRepo(methodRepoConfig)(studentAToken)
                }
                assertExceptionStatusCode(copyFromMethodRepoException, 403)
              }
            }(ownerAuthToken)
          }(ownerAuthToken)
        }
      }

      "to launch workflows" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("reader-launch-workflow-fails"), aclEntries = List(AclEntry(studentA.email, WorkspaceAccessLevel.Reader))) { workspaceName =>
            Rawls.entities.importMetaData(projectName, workspaceName, entity)(ownerAuthToken)

            withMethod("reader-cannot-launch-workflows", MethodData.SimpleMethod) { methodName =>
              val method = MethodData.SimpleMethod.copy(methodName = methodName)

              Rawls.methodConfigs.createMethodConfigInWorkspace(projectName, workspaceName,
                method, method.methodNamespace, method.methodName, 1,
                SimpleMethodConfig.inputs, SimpleMethodConfig.outputs, method.rootEntityType)(ownerAuthToken)

              eventually {
                val submissionException = intercept[RestException] {
                  Rawls.submissions.launchWorkflow(projectName, workspaceName, method.methodNamespace, method.methodName, method.rootEntityType, "participant1", "this", false)(studentAToken)
                }
                assertExceptionStatusCode(submissionException, 403)
              }
            }(ownerAuthToken)
          }(ownerAuthToken)
        }
      }
    }

    "should allow writers" - {
      "to launch workflows if they have can-compute permission" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("writer-can-launch-workflow"), aclEntries = List(AclEntry(studentA.email, WorkspaceAccessLevel.Writer, canCompute = Some(true)))) { workspaceName =>
            Rawls.entities.importMetaData(projectName, workspaceName, entity)(ownerAuthToken)

            withMethod("writer-method-succeeds", MethodData.SimpleMethod) { methodName =>
              val method = MethodData.SimpleMethod.copy(methodName = methodName)

              Rawls.methodConfigs.createMethodConfigInWorkspace(projectName, workspaceName,
                method, method.methodNamespace, method.methodName, 1,
                SimpleMethodConfig.inputs, SimpleMethodConfig.outputs, method.rootEntityType)(ownerAuthToken)

              Orchestration.methods.setMethodPermissions(
                method.methodNamespace,
                method.methodName,
                method.snapshotId,
                studentA.email,
                "OWNER"
              )(ownerAuthToken)

              val submissionId = Rawls.submissions.launchWorkflow(projectName, workspaceName, method.methodNamespace, method.methodName, method.rootEntityType, "participant1", "this", false)(studentAToken)
              // make sure the submission has not errored out
              eventually {
                val submissionStatus = Rawls.submissions.getSubmissionStatus(projectName, workspaceName, submissionId)(studentAToken)._1
                List("Accepted", "Evaluating", "Submitting", "Submitted") should contain (submissionStatus)
              }
            }(ownerAuthToken)
          }(ownerAuthToken)
        }
      }
    }

    "should not allow writers" - {
      "to launch workflows if they don't have can-compute permission" in {
        withCleanBillingProject(owner) { projectName =>
          withWorkspace(projectName, prependUUID("writer-cannot-launch-workflow"), aclEntries = List(AclEntry(studentA.email, WorkspaceAccessLevel.Writer, canCompute = Some(false)))) { workspaceName =>
            Rawls.entities.importMetaData(projectName, workspaceName, entity)(ownerAuthToken)

            withMethod("writer-method-fails", MethodData.SimpleMethod) { methodName =>
              val method = MethodData.SimpleMethod.copy(methodName = methodName)

              Rawls.methodConfigs.createMethodConfigInWorkspace(projectName, workspaceName,
                method, method.methodNamespace, method.methodName, 1,
                SimpleMethodConfig.inputs, SimpleMethodConfig.outputs, method.rootEntityType)(ownerAuthToken)

              Orchestration.methods.setMethodPermissions(
                method.methodNamespace,
                method.methodName,
                method.snapshotId,
                studentA.email,
                "OWNER"
              )(ownerAuthToken)

              val submissionException = intercept[RestException] {
                Rawls.submissions.launchWorkflow(projectName, workspaceName, method.methodNamespace, method.methodName, method.rootEntityType, "participant1", "this", false)(studentAToken)
              }
              assertExceptionStatusCode(submissionException, 403)
            }(ownerAuthToken)
          }(ownerAuthToken)
        }
      }
    }
  }

  private def workspaceResponse(response: String): WorkspaceResponse = response.parseJson.convertTo[WorkspaceResponse]

  private def assertNoAccessToWorkspace(projectName: String, workspaceName: String)(implicit token: AuthToken): Unit = {
    eventually {
      val exception = intercept[RestException](Rawls.workspaces.getWorkspaceDetails(projectName, workspaceName)(token))
      assertExceptionStatusCode(exception, 404)
    }
  }

  private def assertExceptionStatusCode(exception: RestException, statusCode: Int): Unit = {
    exception.message.parseJson.asJsObject.fields("statusCode").convertTo[Int] should be(statusCode)
  }

  private def prependUUID(suffix: String): String = { s"${UUID.randomUUID().toString()}-$suffix" }
}

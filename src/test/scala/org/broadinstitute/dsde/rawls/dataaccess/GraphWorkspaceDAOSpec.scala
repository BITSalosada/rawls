package org.broadinstitute.dsde.rawls.dataaccess

import com.tinkerpop.blueprints.impls.orient.OrientVertex
import org.broadinstitute.dsde.rawls.RawlsException
import org.broadinstitute.dsde.rawls.graph.OrientDbTestFixture
import org.broadinstitute.dsde.rawls.model._
import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.JavaConversions._

class GraphWorkspaceDAOSpec extends FlatSpec with Matchers with OrientDbTestFixture {

  "GraphWorkspaceDAO" should "save a new workspace" in withDefaultTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      val workspace2 = Workspace(
        namespace = testData.wsName.namespace,
        name = testData.wsName.name,
        realm = None,
        workspaceId = "aWorkspaceId",
        bucketName = "aBucket",
        createdDate = testDate,
        lastModified = testDate,
        createdBy = "Barack Obama",
        attributes = Map("workspace_attrib" -> AttributeString("foo")),
        accessLevels = Map.empty,
        realmACLs = Map.empty
      )

      workspaceDAO.save(workspace2, txn)
      // now check explicitly that the vertex exists. note that this will fail if our reserved keywords change.
      assert {
        txn.withGraph { graph =>
          graph.getVertices.exists(v => {
            v.asInstanceOf[OrientVertex].getRecord.getClassName.equalsIgnoreCase(VertexSchema.Workspace) &&
            v.getProperty[String]("name") == workspace2.name && v.getProperty[String]("namespace") == workspace2.namespace
          })
        }
      }
    }
  }

  it should "save a new workspace with a realm" in withEmptyTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      val user = RawlsUser(RawlsUserSubjectId("user@secret.com"), RawlsUserEmail("user@secret.com"))
      authDAO.saveUser(user, txn)
      val secretGroup = makeRawlsGroup("SecretRealm", Set(user), Set.empty)
      authDAO.saveGroup(secretGroup, txn)
      val secretGroupRef = RawlsGroupRef(groupName = secretGroup.groupName)

      val secretWorkspace = Workspace(
        namespace = testData.wsName.namespace,
        name = testData.wsName.name,
        realm = Some(secretGroupRef),
        workspaceId = "aWorkspaceId",
        bucketName = "aBucket",
        createdDate = testDate,
        lastModified = testDate,
        createdBy = "Secret User",
        attributes = Map("workspace_attrib" -> AttributeString("foo")),
        accessLevels = Map.empty,
        realmACLs = Map.empty
      )
      workspaceDAO.save(secretWorkspace, txn)
      assertResult(secretWorkspace) {
        workspaceDAO.findById(secretWorkspace.workspaceId, txn).get.workspace.copy(lastModified = testDate)
      }
    }
  }

  it should "fail to save a new workspace with a missing realm" in withEmptyTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      val user = RawlsUser(RawlsUserSubjectId("user@secret.com"), RawlsUserEmail("user@secret.com"))
      val secretGroup = makeRawlsGroup("SecretRealm", Set(user), Set.empty)
      val secretWorkspace = Workspace(
        namespace = testData.wsName.namespace,
        name = testData.wsName.name,
        realm = Some(secretGroup),
        workspaceId = "aWorkspaceId",
        bucketName = "aBucket",
        createdDate = testDate,
        lastModified = testDate,
        createdBy = "Secret User",
        attributes = Map("workspace_attrib" -> AttributeString("foo")),
        accessLevels = Map.empty,
        realmACLs = Map.empty
      )

      intercept[RawlsException] {
        workspaceDAO.save(secretWorkspace, txn)
      }
    }
  }

  it should "save updates to an existing workspace" in withDefaultTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      // for now, just save the workspace again (making sure it doesn't crash)
      workspaceDAO.save(testData.workspace, txn)
    }
  }

  it should "load a workspace" in withDefaultTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      assertResult(Some(testData.workspace.copy(lastModified = testDate))) {
        workspaceDAO.loadContext(testData.workspace.toWorkspaceName, txn) map( _.workspace.copy(lastModified = testDate) )
      }
    }
  }

  it should "return None when a workspace does not exist" in withDefaultTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      assertResult(None) {
        new GraphWorkspaceDAO().loadContext(WorkspaceName(testData.workspace.namespace, "fnord"), txn)
      }
      assertResult(None) {
        new GraphWorkspaceDAO().loadContext(WorkspaceName("fnord", testData.workspace.name), txn)
      }
    }
  }

  //what is a "short" workspace and why is this test the exact same as "load a workspace" from 2 tests above?
  it should "load the short version of a workspace" in withDefaultTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      assertResult(Some(testData.workspace.copy(lastModified = testDate))) {
        workspaceDAO.loadContext(testData.workspace.toWorkspaceName, txn) map( _.workspace.copy(lastModified = testDate) )
      }
    }
  }

  it should "show workspace in list" in withDefaultTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      //we need to use a default time because we have no idea when the workspace was actually last modified, so the assert would fail otherwise
      val dateTime = DateTime.now
      assert {
        workspaceDAO.list(txn).map(ws => ws.copy(lastModified = dateTime)).contains(testData.workspace.copy(lastModified = dateTime))
      }
    }
  }

  it should "fail when attempting to put dots in attribute keys" in withDefaultTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      val dottyWorkspace = Workspace(
        namespace = testData.wsName.namespace,
        name = "badness",
        realm = None,
        workspaceId = "badWorkspaceId",
        bucketName = "badBucket",
        createdDate = testDate,
        lastModified = testDate,
        createdBy = "Mitt Romney",
        attributes = Map("dots.dots.more.dots" -> AttributeString("foo")),
        accessLevels = Map.empty,
        realmACLs = Map.empty
      )

      intercept[RawlsException] {
        workspaceDAO.save(dottyWorkspace, txn)
      }
    }
  }

  it should "change lastModified date on a workspace after altering it" in withDefaultTestDatabase { dataSource =>
    dataSource.inTransaction() { txn =>
      val beforeUpdate = workspaceDAO.loadContext(testData.workspace.toWorkspaceName, txn).get.workspace.lastModified
      workspaceDAO.save(testData.workspace.copy(attributes = testData.wsAttrs), txn)

      Thread.sleep(10) //travis is blazing fast, so sleep to prevent it from saving and updating workspaces at the same moment

      val afterUpdate = workspaceDAO.loadContext(testData.workspace.toWorkspaceName, txn).get.workspace.lastModified

      assert(afterUpdate.getMillis > beforeUpdate.getMillis)
    }
  }

  Attributable.reservedAttributeNames.foreach { reserved =>
    it should "fail using reserved attribute name " + reserved in withDefaultTestDatabase { dataSource =>
      val e = Workspace(
        namespace = testData.wsName.namespace,
        name = "badness",
        realm = None,
        workspaceId = "badWorkspaceId",
        bucketName = "badBucket",
        createdDate = testDate,
        lastModified = testDate,
        createdBy = "Mitt Romney",
        attributes = Map(reserved -> AttributeString("foo")),
        accessLevels = Map.empty,
        realmACLs = Map.empty
      )
      dataSource.inTransaction(writeLocks=Set(testData.workspace.toWorkspaceName)) { txn =>
        withWorkspaceContext(testData.workspace, txn) { context =>
          intercept[RawlsException] { workspaceDAO.save(e, txn) }
        }
      }
    }
  }

  it should "save, load, and modify PendingBucketDeletions" in withDefaultTestDatabase { dataSource =>
    val initPendingSet = Set("bucket1", "bucket2", "bucket3")
    val initPending = PendingBucketDeletions(initPendingSet)

    dataSource.inTransaction() { txn =>
      workspaceDAO.savePendingBucketDeletions(initPending, txn)
    }

    dataSource.inTransaction() { txn =>
      assertResult(initPending) {
        workspaceDAO.loadPendingBucketDeletions(txn).get
      }
    }

    val newBucket = "bucket4"
    val setPlusOne = initPendingSet + newBucket
    val pendingPlusOne = PendingBucketDeletions(setPlusOne)

    dataSource.inTransaction() { txn =>
      workspaceDAO.savePendingBucketDeletions(initPending.copy(buckets = initPending.buckets + newBucket), txn)
    }

    dataSource.inTransaction() { txn =>
      assertResult(pendingPlusOne) {
        workspaceDAO.loadPendingBucketDeletions(txn).get
      }
    }

    dataSource.inTransaction() { txn =>
      workspaceDAO.savePendingBucketDeletions(pendingPlusOne.copy(buckets = initPending.buckets - newBucket), txn)
    }

    dataSource.inTransaction() { txn =>
      assertResult(initPending) {
        workspaceDAO.loadPendingBucketDeletions(txn).get
      }
    }

  }

}

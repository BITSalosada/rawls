package org.broadinstitute.dsde.rawls.dataaccess.slick

import java.util.UUID

import org.broadinstitute.dsde.rawls.model._

class SQLLogSpec extends TestDriverComponentWithFlatSpecAndMatchers {

  // 1. create entity
  // 2. update entity
  // 3. copy entities to empty workspace
  // 4. copy entities to existing workspace

  "SQLLogSpec" should "exercise some queries for analysis" in withDefaultTestDatabase {

    // 1. create entity

    withWorkspaceContext(testData.workspace) { context =>
      val attrs = Map(
        AttributeName.withDefaultNS("attr1") -> AttributeString("yes"),
        AttributeName.withDefaultNS("attr2") -> AttributeNumber(5),
        AttributeName.withDefaultNS("attr3") -> AttributeBoolean(false),
        AttributeName.withDefaultNS("attr4") -> AttributeEntityReferenceEmptyList
      )
      val ent = Entity("testEnt", "testType", attrs)
      runAndWait(entityQuery.save(context, ent))
    }

    // 2. update entity

    withWorkspaceContext(testData.workspace) { context =>
      val newAttrs = Map(
        AttributeName.withDefaultNS("attr2") -> AttributeString("not a number"),
        AttributeName.withDefaultNS("attr3") -> AttributeBoolean(false),
        AttributeName.withDefaultNS("attr4") -> AttributeEntityReferenceEmptyList,
        AttributeName.withDefaultNS("attr5") -> AttributeString("a new one")
      )
      val ent = Entity("testEnt", "testType", newAttrs)
      runAndWait(entityQuery.save(context, ent))
    }

    val newWorkspace = Workspace(testData.workspace.namespace, "newForTest", Set.empty, UUID.randomUUID.toString, "bucketname", currentTime(), currentTime(), "me", Map.empty, Map.empty, Map.empty)
    runAndWait(workspaceQuery.save(newWorkspace))

    // 3. copy entities to empty workspace

    withWorkspaceContext(testData.workspace) { srcContext =>
      withWorkspaceContext(newWorkspace) { destContext =>
        runAndWait(entityQuery.copyAllEntities(srcContext, destContext))
      }
    }

    // 4. copy entities to existing workspace

    withWorkspaceContext(testData.workspace) { srcContext =>

      val newAttrs = Map(
        AttributeName.withDefaultNS("attr2") -> AttributeString("not a number"),
        AttributeName.withDefaultNS("attr3") -> AttributeBoolean(false),
        AttributeName.withDefaultNS("attr4") -> AttributeEntityReferenceEmptyList,
        AttributeName.withDefaultNS("attr5") -> AttributeString("a new one")
      )
      val newEnt = Entity("newEnt", "testType", newAttrs)
      runAndWait(entityQuery.save(srcContext, newEnt))

      withWorkspaceContext(newWorkspace) { destContext =>
        runAndWait(entityQuery.checkAndCopyEntities(srcContext, destContext, "testType", Seq("testEnt", "newEnt", "missing"), false))
        runAndWait(entityQuery.checkAndCopyEntities(srcContext, destContext, "testType", Seq("testEnt", "newEnt", "missing"), true))
      }
    }

  }
}
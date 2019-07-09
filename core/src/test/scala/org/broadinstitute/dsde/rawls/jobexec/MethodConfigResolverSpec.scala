package org.broadinstitute.dsde.rawls.jobexec

import org.broadinstitute.dsde.rawls.RawlsException
import org.broadinstitute.dsde.rawls.model._

import scala.collection.immutable.Map
import org.scalatest.{Matchers, WordSpecLike}
import org.broadinstitute.dsde.rawls.dataaccess.slick._
import org.broadinstitute.dsde.rawls.dataaccess.SlickWorkspaceContext
import java.util.UUID

import org.broadinstitute.dsde.rawls.jobexec.MethodConfigResolver.GatherInputsResult
import scala.concurrent.ExecutionContext
import org.broadinstitute.dsde.rawls.dataaccess.MockCromwellSwaggerClient._

class MethodConfigResolverSpec extends WordSpecLike with Matchers with TestDriverComponent {

  import driver.api._

  val littleWdl =
    """
      |task t1 {
      |  Int int_arg
      |  Int? int_opt
      |  command {
      |    echo ${int_arg}
      |    echo ${int_opt}
      |  }
      |}
      |
      |workflow w1 {
      |  call t1
      |}
    """.stripMargin


  val arrayWdl =
    """
      |task t1 {
      |  Int int_arg
      |  command {
      |    echo ${int_arg}
      |  }
      |}
      |
      |workflow w1 {
      |  Array[Int] int_array
      |  scatter(i in int_array) {
      |    call t1 { input: int_arg = i }
      |  }
      |}
    """.stripMargin



  val doubleArrayWdl =
    """
      |task t1 {
      |  Array[Int] aint_arg
      |  command {
      |    echo ${aint_arg}
      |  }
      |}
      |
      |workflow w1 {
      |  Array[Array[Int]] aint_array
      |  scatter(ai in aint_array) {
      |    call t1 { input: aint_arg = ai }
      |  }
      |}
    """.stripMargin


  val optionalDoubleArrayWdl =
    """
      |task t1 {
      |  Array[Int] aint_arg
      |  command {
      |    echo ${aint_arg}
      |  }
      |}
      |
      |workflow w1 {
      |  Array[Array[Int]]? aint_array
      |  scatter(ai in aint_array) {
      |    call t1 { input: aint_arg = ai }
      |  }
      |}
    """.stripMargin


  val tripleArrayWdl =
    """
      |task t1 {
      |  Array[Array[Int]] aint_arg
      |  command {
      |    echo ${aint_arg}
      |  }
      |}
      |
      |workflow w1 {
      |  Array[Array[Array[Int]]] aaint_array
      |  scatter(ai in aaint_array) {
      |    call t1 { input: aint_arg = ai }
      |  }
      |}
    """.stripMargin

  val badWdl = littleWdl.replace("workflow", "not-a-workflow")

  val intArgName = "w1.t1.int_arg"
  val intOptName = "w1.t1.int_opt"
  val intArrayName = "w1.int_array"
  val doubleIntArrayName = "w1.aint_array"
  val tripleIntArrayName = "w1.aaint_array"

  val littleWdlWorkflowDescriptionRequiredInput = makeToolInputParameter(intArgName, false, makeValueType("Int"), "Int")
  val littleWdlWorkflowDescriptionOptionalInput = makeToolInputParameter(intOptName, true, makeValueType("Int"), "Int?")
  val littleWdlWorkflowDescription = makeWorkflowDescription("", List(littleWdlWorkflowDescriptionRequiredInput, littleWdlWorkflowDescriptionOptionalInput), List.empty)

  val requiredArrayInput = makeToolInputParameter(intArrayName, false, makeArrayValueType(makeValueType("Int")), "Array[Int]")
  val requiredArrayWorkflowDescription = makeWorkflowDescription("", List(requiredArrayInput), List.empty)

  val requiredDoubleArrayInput =   makeToolInputParameter(doubleIntArrayName, false, makeArrayValueType(makeArrayValueType(makeValueType("Int"))), "Array[Array[Int]]")
  val requiredDoubleArrayWorkflowDescription =  makeWorkflowDescription("", List(requiredDoubleArrayInput), List.empty)

  val optionalDoubleArrayInput =   makeToolInputParameter(doubleIntArrayName, false, makeArrayValueType(makeArrayValueType(makeValueType("Int"))), "Array[Array[Int]]")
  val optionalDoubleArrayWorkflowDescription =  makeWorkflowDescription("", List(optionalDoubleArrayInput), List.empty)


  val requiredTripleArrayInput  = makeToolInputParameter(tripleIntArrayName, true, makeArrayValueType(makeArrayValueType(makeArrayValueType(makeValueType("Int")))), "Array[Array[Array[Int]]]")
  val requiredTripleArrayWorkflowDescription =  makeWorkflowDescription("", List(requiredTripleArrayInput), List.empty)

  val badWdlWorkflowDescription = makeBadWorkflowDescription("badwdl", List("ERROR: Finished parsing without consuming all tokens.\\n\\nnot-a-workflow w1 {\\n^\\n    "))

  mockCromwellSwaggerClient.workflowDescriptions += (littleWdl -> littleWdlWorkflowDescription)
  mockCromwellSwaggerClient.workflowDescriptions += (arrayWdl  -> requiredArrayWorkflowDescription)
  mockCromwellSwaggerClient.workflowDescriptions += (doubleArrayWdl -> requiredDoubleArrayWorkflowDescription)
  mockCromwellSwaggerClient.workflowDescriptions += (optionalDoubleArrayWdl -> optionalDoubleArrayWorkflowDescription)
  mockCromwellSwaggerClient.workflowDescriptions += (tripleArrayWdl -> requiredTripleArrayWorkflowDescription)
  mockCromwellSwaggerClient.workflowDescriptions += (badWdl -> badWdlWorkflowDescription)


  val workspace = Workspace("workspaces", "test_workspace", UUID.randomUUID().toString(), "aBucket", Some("workflow-collection"), currentTime(), currentTime(), "testUser", Map.empty)

  import spray.json._
  val sampleGood = Entity("sampleGood", "Sample",
    Map(AttributeName.withDefaultNS("blah") -> AttributeNumber(1),
        AttributeName.withDefaultNS("rawJsonDoubleArray") -> AttributeValueRawJson( "[[0,1,2],[3,4,5]]".parseJson)))
  val sampleGood2 = Entity("sampleGood2", "Sample",
    Map(AttributeName.withDefaultNS("blah") -> AttributeNumber(2),
        AttributeName.withDefaultNS("rawJsonDoubleArray") -> AttributeValueRawJson( "[[3,4,5],[6,7,8]]".parseJson)))
  val sampleMissingValue = Entity("sampleMissingValue", "Sample", Map.empty)

  val sampleSet = Entity("daSampleSet", "SampleSet",
    Map(AttributeName.withDefaultNS("samples") -> AttributeEntityReferenceList(Seq(
      sampleGood.toReference,
      sampleMissingValue.toReference)
    ))
  )

  val sampleSet2 = Entity("daSampleSet2", "SampleSet",
    Map(AttributeName.withDefaultNS("samples") -> AttributeEntityReferenceList(Seq(
      sampleGood.toReference,
      sampleGood2.toReference
    )),
      AttributeName.withDefaultNS("rawJsonDoubleArray") -> AttributeValueRawJson( "[[0,1,2],[3,4,5]]".parseJson )
    )
  )

  val sampleSet3 = Entity("daSampleSet3", "SampleSet",
    Map(AttributeName.withDefaultNS("samples") -> AttributeEntityReferenceList(Seq(
      sampleGood.toReference))))

  val dummyMethod = AgoraMethod("method_namespace", "test_method", 1)

  val configGood = MethodConfiguration("config_namespace", "configGood", Some("Sample"),
    None, Map(intArgName -> AttributeString("this.blah")), Map.empty, dummyMethod)

  val configEvenBetter = MethodConfiguration("config_namespace", "configGood", Some("Sample"),
    None, Map(intArgName -> AttributeString("this.blah"), intOptName -> AttributeString("this.blah")),
    Map.empty, dummyMethod)

  val configMissingExpr = MethodConfiguration("config_namespace", "configMissingExpr", Some("Sample"),
    None, Map.empty, Map.empty, dummyMethod)

  val configSampleSet = MethodConfiguration("config_namespace", "configSampleSet", Some("SampleSet"),
    None, Map(intArrayName -> AttributeString("this.samples.blah")), Map.empty, dummyMethod)

  val configEmptyArray = MethodConfiguration("config_namespace", "configSampleSet", Some("SampleSet"),
    None, Map(intArrayName -> AttributeString("this.nonexistent")), Map.empty, dummyMethod)

  val configRawJsonDoubleArray = MethodConfiguration("config_namespace", "configSampleSet", Some("SampleSet"),
    None, Map(doubleIntArrayName -> AttributeString("this.rawJsonDoubleArray")), Map.empty, dummyMethod)

  val configRawJsonTripleArray = MethodConfiguration("config_namespace", "configSample", Some("Sample"),
    None, Map(tripleIntArrayName -> AttributeString("this.samples.rawJsonDoubleArray")), Map.empty, dummyMethod)

  class ConfigData extends TestData {
    override def save() = {
      DBIO.seq(
        workspaceQuery.save(workspace),
        withWorkspaceContext(workspace) { context =>
          DBIO.seq(
            entityQuery.save(context, sampleGood),
            entityQuery.save(context, sampleGood2),
            entityQuery.save(context, sampleMissingValue),
            entityQuery.save(context, sampleSet),
            entityQuery.save(context, sampleSet2),
            entityQuery.save(context, sampleSet3),
            methodConfigurationQuery.create(context, configGood),
            methodConfigurationQuery.create(context, configMissingExpr),
            methodConfigurationQuery.create(context, configSampleSet)
          )
        }
      )
    }
  }

  val configData = new ConfigData()

  def withConfigData[T](testCode: => T): T = {
    withCustomTestDatabaseInternal(configData)(testCode)
  }




  //Test harness to call resolveInputsForEntities without having to go via the WorkspaceService
  def testResolveInputs(workspaceContext: SlickWorkspaceContext, methodConfig: MethodConfiguration, entity: Entity, wdl: String, dataAccess: DataAccess)
                       (implicit executionContext: ExecutionContext): ReadWriteAction[Map[String, Seq[SubmissionValidationValue]]] = {
    dataAccess.entityQuery.findEntityByName(workspaceContext.workspaceId, entity.entityType, entity.name).result flatMap { entityRecs =>
      methodConfigResolver.gatherInputs(userInfo, methodConfig, wdl) match {
        case scala.util.Failure(exception) =>
          DBIO.failed(exception)
        case scala.util.Success(gatherInputsResult: GatherInputsResult)
          if gatherInputsResult.extraInputs.nonEmpty || gatherInputsResult.missingInputs.nonEmpty =>
          DBIO.failed(new RawlsException(s"gatherInputsResult has missing or extra inputs: $gatherInputsResult"))
        case scala.util.Success(gatherInputsResult: GatherInputsResult) =>
          methodConfigResolver.evaluateInputExpressions(workspaceContext, gatherInputsResult.processableInputs, Some(entityRecs), dataAccess)
      }
    }
  }


  "MethodConfigResolver" should {
    "resolve method config inputs" in withConfigData {
      val context = SlickWorkspaceContext(workspace)

      runAndWait(testResolveInputs(context, configGood, sampleGood, littleWdl, this)) shouldBe
        Map(sampleGood.name -> Seq(SubmissionValidationValue(Some(AttributeNumber(1)), None, intArgName)))

      runAndWait(testResolveInputs(context, configEvenBetter, sampleGood, littleWdl, this)) shouldBe
        Map(sampleGood.name -> Seq(SubmissionValidationValue(Some(AttributeNumber(1)), None, intArgName), SubmissionValidationValue(Some(AttributeNumber(1)), None, intOptName)))

      runAndWait(testResolveInputs(context, configSampleSet, sampleSet, arrayWdl, this)) shouldBe
        Map(sampleSet.name -> Seq(SubmissionValidationValue(Some(AttributeValueList(Seq(AttributeNumber(1)))), None, intArrayName)))

      runAndWait(testResolveInputs(context, configSampleSet, sampleSet2, arrayWdl, this)) shouldBe
        Map(sampleSet2.name -> Seq(SubmissionValidationValue(Some(AttributeValueList(Seq(AttributeNumber(1), AttributeNumber(2)))), None, intArrayName)))

      // failure cases
      assertResult(true, "Missing values should return an error") {
        runAndWait(testResolveInputs(context, configGood, sampleMissingValue, littleWdl, this)).get("sampleMissingValue").get match {
          case List(SubmissionValidationValue(None, Some(_), intArg)) if intArg == intArgName => true
        }
      }

      //MethodConfiguration config_namespace/configMissingExpr is missing definitions for these inputs: w1.t1.int_arg
      intercept[RawlsException] {
        runAndWait(testResolveInputs(context, configMissingExpr, sampleGood, littleWdl, this))
      }
    }

    "remove missing inputs from processable inputs in GatherInputsResult" in withConfigData {
      val gatheredInputs = methodConfigResolver.gatherInputs(userInfo, configMissingExpr, littleWdl)
      gatheredInputs shouldBe 'success
      gatheredInputs.get.processableInputs shouldBe 'empty
      gatheredInputs.get.missingInputs shouldBe Set(intArgName)
      gatheredInputs.get.emptyOptionalInputs.map(_.workflowInput.getName) shouldBe Set(intOptName)
    }

    "resolve empty lists into AttributeEmptyLists" in withConfigData {
      val context = SlickWorkspaceContext(workspace)

      runAndWait(testResolveInputs(context, configEmptyArray, sampleSet2, arrayWdl, this)) shouldBe
        Map(sampleSet2.name -> Seq(SubmissionValidationValue(Some(AttributeValueEmptyList), None, intArrayName)))
    }

    "unpack AttributeValueRawJson into WDL-arrays" in withConfigData {
      val context = SlickWorkspaceContext(workspace)

      val resolvedInputs: Map[String, Seq[SubmissionValidationValue]] = runAndWait(testResolveInputs(context, configRawJsonDoubleArray, sampleSet2, doubleArrayWdl, this))
      val methodProps = resolvedInputs(sampleSet2.name).map { svv: SubmissionValidationValue =>
        svv.inputName -> svv.value.get
      }
      val wdlInputs: String = methodConfigResolver.propertiesToWdlInputs(methodProps.toMap)

      wdlInputs shouldBe """{"w1.aint_array":[[0,1,2],[3,4,5]]}"""
    }

    "unpack AttributeValueRawJson into optional WDL-arrays" in withConfigData {
      val context = SlickWorkspaceContext(workspace)

      val resolvedInputs: Map[String, Seq[SubmissionValidationValue]] = runAndWait(testResolveInputs(context, configRawJsonDoubleArray, sampleSet2, optionalDoubleArrayWdl, this))
      val methodProps = resolvedInputs(sampleSet2.name).map { svv: SubmissionValidationValue =>
        svv.inputName -> svv.value.get
      }
      val wdlInputs: String = methodConfigResolver.propertiesToWdlInputs(methodProps.toMap)

      wdlInputs shouldBe """{"w1.aint_array":[[0,1,2],[3,4,5]]}"""
    }

    "unpack AttributeValueRawJson into lists-of WDL-arrays" in withConfigData {
      val context = SlickWorkspaceContext(workspace)

      val resolvedInputs: Map[String, Seq[SubmissionValidationValue]] = runAndWait(testResolveInputs(context, configRawJsonTripleArray, sampleSet2, tripleArrayWdl, this))
      val methodProps = resolvedInputs(sampleSet2.name).map { svv: SubmissionValidationValue =>
        svv.inputName -> svv.value.get
      }
      val wdlInputs: String = methodConfigResolver.propertiesToWdlInputs(methodProps.toMap)

      wdlInputs shouldBe """{"w1.aaint_array":[[[0,1,2],[3,4,5]],[[3,4,5],[6,7,8]]]}"""
    }

     /* IGNORED - Failure case.
        This is the failure case described in MethodConfigResolver.getArrayResult.
    "unpack AttributeValueRawJson into single-element lists-of WDL-arrays" in withConfigData {
      val context = SlickWorkspaceContext(workspace)

      val resolvedInputs: Map[String, Seq[SubmissionValidationValue]] = runAndWait(testResolveInputs(context, configRawJsonTripleArray, sampleSet3, tripleArrayWdl, this))
      val methodProps = resolvedInputs(sampleSet3.name).map { svv: SubmissionValidationValue =>
        svv.inputName -> svv.value.get
      }
      val wdlInputs: String = MethodConfigResolver.propertiesToWdlInputs(methodProps.toMap)

      wdlInputs shouldBe """{"w1.aaint_array":[[[0,1,2],[3,4,5]]]}"""
      //actually returns: {"w1.aaint_array":[[0,1,2],[3,4,5]]}
      //(note the scalatest output adds an extra set of square brackets to everything for no reason i can discern)
    }
    */

    "parse WDL" in withConfigData {
      val littleWorkflow = methodConfigResolver.parseWDL(userInfo, littleWdl).get

      assertResult(littleWdlWorkflowDescription) {
        littleWorkflow
      }

      val arrayWorkflow = methodConfigResolver.parseWDL(userInfo, arrayWdl).get

      assertResult(requiredArrayWorkflowDescription) {
        arrayWorkflow
      }
    }

    "parse WDL with syntax errors" in withConfigData {

      val badWdlParse = methodConfigResolver.parseWDL(userInfo, badWdl)


      assert(badWdlParse.isSuccess)
      assertResult(badWdlWorkflowDescription) {
        badWdlParse.get
      }

    }

    "get method config inputs and outputs" in withConfigData {
      val expectedLittleIO = MethodInputsOutputs(Seq(
        MethodInput(intArgName, "Int", false),
        MethodInput(intOptName, "Int?", true)), Seq())

      assertResult(expectedLittleIO) {
        methodConfigResolver.getMethodInputsOutputs(userInfo, littleWdl).get
      }

      val expectedArrayIO = MethodInputsOutputs(Seq(
        MethodInput(intArrayName, "Array[Int]", false)), Seq())

      assertResult(expectedArrayIO) {
        methodConfigResolver.getMethodInputsOutputs(userInfo, arrayWdl).get
      }

      val badIO = methodConfigResolver.getMethodInputsOutputs(userInfo, badWdl)
      assert(badIO.isFailure)
      intercept[RawlsException] {
        badIO.get
      }
    }

    "create a Method Config from a template" in withConfigData {
      val expectedLittleInputs = Map(intArgName -> AttributeString(""), intOptName -> AttributeString(""))
      val expectedLittleTemplate = MethodConfiguration("namespace", "name", Some("rootEntityType"), Some(Map()), expectedLittleInputs, Map(), dummyMethod)

      assertResult(expectedLittleTemplate) { methodConfigResolver.toMethodConfiguration(userInfo, littleWdl, dummyMethod).get }

      val expectedArrayInputs = Map(intArrayName -> AttributeString(""))
      val expectedArrayTemplate = MethodConfiguration("namespace", "name", Some("rootEntityType"), Some(Map()), expectedArrayInputs, Map(), dummyMethod)

      assertResult(expectedArrayTemplate) { methodConfigResolver.toMethodConfiguration(userInfo, arrayWdl, dummyMethod).get }

      val badTemplate = methodConfigResolver.toMethodConfiguration(userInfo, badWdl, dummyMethod)
      assert(badTemplate.isFailure)
      intercept[RawlsException] {
        badTemplate.get
      }
    }
  }
}

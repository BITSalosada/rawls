package default

import scala.concurrent.duration._

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.jdbc.Predef._

class createWorkspaces100 extends Simulation {

	val accessToken = "YOUR_ACCESS_TOKEN" //place your token here :)

	//function to help us generate TSVs per-run
	def generateTSV(f: java.io.File)(op: java.io.PrintWriter => Unit) {
	  val p = new java.io.PrintWriter(f)
	  try { op(p) } finally { p.close() }
	}

	val httpProtocol = http
		.baseURL("https://rawls.dsde-dev.broadinstitute.org")
		.inferHtmlResources()
	//	.extraInfoExtractor(extraInfo => List(extraInfo.response)) //for when we want to extract additional info for the simulation.log

	val headers = Map("Authorization" -> s"Bearer ${accessToken}",
						"Content-Type" -> "application/json") 

	//generate the TSV to use for this run
	generateTSV(new File("../user-files/data/createWorkspaces100.tsv")) { p =>
		val r = scala.util.Random
		val runID = s"gatling_100clones_${r.nextInt(999999999)}"

		p.println("workspaceJson")

		val i = 0
		for(i <- 1 to 100){
			p.println(s""""{""namespace"":""broad-dsde-dev"",""name"":""${runID}_${i}"",""attributes"":""{}""}"""")
		}
	}

	val scn = scenario("createWorkspaces100")
		.feed(tsv("../user-files/data/createWorkspaces100.tsv")) //the tsv from generateTSV
		.exec(http("create_request")
			.post("/api/workspaces")
			.headers(headers_0)
			.body(StringBody("${workspaceJson}"))) //feeds off of the workspaceJson column in the csv file

	setUp(scn.inject(rampUsers(100) over(60 seconds))).protocols(httpProtocol) //ramp up 100 users over 60 seconds
}
package default

import io.gatling.core.Predef._

trait RawlsSimulation extends Simulation {

  //function to help us generate TSVs per-run
  def fileGenerator(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  //Helpers to set up the run

  val lines = scala.io.Source.fromFile("../user-files/config.txt").getLines
  val accessToken = lines.next
  val numUsers = lines.next.toInt

}
package org.broadinstitute.dsde.rawls.metrics

import spray.routing.Directive0
import spray.routing.directives.MiscDirectives.requestInstance
import spray.routing.directives.BasicDirectives.pass

trait InstrumentationDirectives extends RawlsInstrumented {

  override val rawlsMetricBaseName = "rawls"

  // extract the request path from the URI, increment the counter, and continue processing the request

  // TODO can I add timing here (Anu's story) - google "scala spray request timing"

  def instrumentRequestPath: Directive0 = requestInstance flatMap { request =>
    val x = ExpandedMetricBuilder.expand("temp")
    httpRequestCounter(x)(request).inc()
    pass
  }
}

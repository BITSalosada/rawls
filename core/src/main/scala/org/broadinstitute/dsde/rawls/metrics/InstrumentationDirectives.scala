package org.broadinstitute.dsde.rawls.metrics

import nl.grons.metrics.scala.Counter

import spray.routing.Directive0
import spray.routing.directives.MiscDirectives.requestUri
import spray.routing.directives.BasicDirectives.pass
import spray.http.Uri.Path

trait InstrumentationDirectives extends RawlsInstrumented {

  override val rawlsMetricBaseName = "rawls"

  def requestCounter(requestPath: Path): Counter =
    ExpandedMetricBuilder
      .expand(RequestPathMetric, requestPath)
      .asCounter()

  // extract the request path from the URI, increment the counter, and continue processing the request
  def instrumentRequestPath: Directive0 = requestUri flatMap { uri =>
    requestCounter(uri.path).inc()
    pass
  }
}

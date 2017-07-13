package org.broadinstitute.dsde.rawls.metrics

import com.typesafe.scalalogging.LazyLogging
import spray.routing.Directive0
import spray.routing.directives.BasicDirectives._
import spray.routing.directives.MiscDirectives.requestInstance


trait InstrumentationDirectives extends RawlsInstrumented with LazyLogging {

  override val rawlsMetricBaseName = "rawls"

  // extract the request path from the URI, increment the counter, and continue processing the request

  // TODO can I add timing here (Anu's story) - google "scala spray request timing"

  def instrumentRequestPath: Directive0 = requestInstance flatMap { request =>
    logger.info("we're in instrumentRequestPath")
    httpRequestCounter(ExpandedMetricBuilder.expand(rawlsMetricBaseName))(request).inc()

    mapRequestContext { context =>
      logger.info("we have the context:  " + context)
      val timeStamp = System.currentTimeMillis
      context.withHttpResponseMapped { response =>
        logger.info("we're in the response:  " + response.toString)
        val rTime = httpRequestTimer2(request).time(System.currentTimeMillis - timeStamp)
        logger.info("time: " + rTime.toString)
        response
      }
    }
  }

}

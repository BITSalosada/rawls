package org.broadinstitute.dsde.rawls.metrics

import java.util.UUID

import nl.grons.metrics.scala._
import org.broadinstitute.dsde.rawls.model.SubmissionStatuses.SubmissionStatus
import org.broadinstitute.dsde.rawls.model.Subsystems.Subsystem
import org.broadinstitute.dsde.rawls.model.WorkflowStatuses.WorkflowStatus
import org.broadinstitute.dsde.rawls.model.WorkspaceName
import slick.dbio.{DBIOAction, Effect, NoStream}
import spray.http._

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext

/**
  * Mixin trait for instrumentation.
  * Extends metrics-scala [[DefaultInstrumented]] and provide additional utilties for generating
  * metric names for FireCloud.
  */
trait RawlsInstrumented extends DefaultInstrumented {
  // Keys for expanded metric fragments
  final val WorkspaceMetric = "workspace"
  final val SubmissionMetric = "submission"
  final val SubmissionStatusMetric = "submissionStatus"
  final val WorkflowStatusMetric = "workflowStatus"
  final val SubsystemMetric = "subsystem"

  /**
    * Base name for all metrics. This will be prepended to all generated metric names.
    * Example: dev.firecloud.rawls
    */
  protected val rawlsMetricBaseName: String
  override lazy val metricBaseName = MetricName(rawlsMetricBaseName)

  /**
    * Typeclass for something that can be converted into a metric name fragment with a given key.
    * Metric name fragments are combined via ExpandedMetricBuilder to generate an "expanded" metric name.
    * By default this just calls toString on the object of type A, but this can be overridden.
    */
  @implicitNotFound(msg = "Cannot expand instances of type ${A}")
  protected sealed trait Expansion[A] {
    def makeName(key: Option[String], a: A) = s"${getKey(key)}${a.toString}"
    protected def getKey(k: Option[String]): String = k.map(k => s"$k.").getOrElse("")
  }

  // Typeclass instances:

  protected implicit object WorkspaceNameExpansion extends Expansion[WorkspaceName] {
    // Statsd doesn't allow slashes in metric names, so we override makeName to override
    // the default toString based implementation.
    override def makeName(key: Option[String], n: WorkspaceName): String = s"${getKey(key)}${n.toString.replace('/', '.')}"
  }

  // Provide an implicit for UUIDs but can use the default makeName
  protected implicit object UUIDExpansion extends Expansion[UUID]

  // Provide implicits for various RawlsEnumerations using the default makeName.
  // TODO: should we provide an expansion for RawlsEnumeration in general?
  //
  // These both take an upper type bound A <: WorkflowStatus|SubmissionStatus so they can
  // work either with the supertype (e.g. WorkflowStatus) or a subtype (e.g. WorkflowStatuses.Launching).
  protected implicit def WorkflowStatusExpansion[A <: WorkflowStatus] = new Expansion[A] {}
  protected implicit def SubmissionStatusExpansion[A <: SubmissionStatus] = new Expansion[A] {}
  protected implicit def SubsystemExpansion[A <: Subsystem] = new Expansion[A] {}

  protected implicit object HttpMethodExpansion extends Expansion[HttpMethod] {
    override def makeName(key: Option[String], m: HttpMethod): String = s"${getKey(key)}${m.toString.toLowerCase}"
  }

  protected implicit object UriExpansion extends Expansion[Uri] {
    override def makeName(key: Option[String], uri: Uri): String = s"${getKey(key)}${uri2String(uri)}"

    private def uri2String(uri: Uri): String = {
      val path = if (uri.path.startsWithSlash) uri.path.tail.toString else uri.path
      path.toString.replace('/', '_')
    }
  }

  protected implicit object StatusCodeExpansion extends Expansion[StatusCode] {
    override def makeName(key: Option[String], statusCode: StatusCode): String = s"${statusCode.intValue.toString}"
  }

  protected implicit object StringExpansion extends Expansion[String]

  /**
    * Utility for building expanded metric names in a typesafe way. Example usage:
    * {{{
    *   val counter: Counter =
    *     ExpandedMetricBuilder
    *       .expand(WorkspaceMetric, workspaceName)
    *       .expand(SubmissionMetric, submissionId)
    *       .expand(WorkflowStatusMetric, status)
    *       .asCounter("count")
    *   // counter has name:
    *   // <baseName>.workspace.<workspaceNamespace>.<workspaceName>.submission.<submissionId>.workflowStatus.<workflowStatus>.count
    *   counter += 1000
    * }}}
    *
    * Note the above will only compile if there are [[Expansion]] instances for the types passed to the expand method.
    */
  protected class ExpandedMetricBuilder private (m: String = "") {
    def expand[A: Expansion](key: Option[String], a: A): ExpandedMetricBuilder = {
      new ExpandedMetricBuilder(
        (if (m == "") m else m + ".") + implicitly[Expansion[A]].makeName(key, a))
    }

    def expand[A: Expansion](key: String, a: A): ExpandedMetricBuilder = {
      expand(Some(key), a)
    }

    def expand[A: Expansion](a: A): ExpandedMetricBuilder = {
      expand(None, a)
    }

    def asCounter(name: Option[String] = None): Counter =
      metrics.counter(makeName(name))

    def asGauge[T](name: Option[String] = None)(fn: => T): Gauge[T] =
      metrics.gauge(makeName(name))(fn)

    def asTimer(name: Option[String] = None): Timer =
      metrics.timer(makeName(name))

    private def makeName(name: Option[String]): String = {
      m + name.map(n => s".$n").getOrElse("")
    }

    override def toString: String = m
  }

  object ExpandedMetricBuilder {
    def expand[A: Expansion](key: Option[String], a: A): ExpandedMetricBuilder = {
      new ExpandedMetricBuilder().expand(key, a)
    }

    def expand[A: Expansion](key: String, a: A): ExpandedMetricBuilder = {
      expand(Some(key), a)
    }

    def expand[A: Expansion](a: A): ExpandedMetricBuilder = {
      expand(None, a)
    }
  }

  // Handy definitions which can be used by implementing classes:

  /**
    * An ExpandedMetricBuilder for a WorkspaceName.
    */
  protected def workspaceMetricBuilder(workspaceName: WorkspaceName): ExpandedMetricBuilder =
    ExpandedMetricBuilder.expand(WorkspaceMetric, workspaceName)

  /**
    * An ExpandedMetricBuilder for a WorkspaceName and a submission ID.
    */
  protected def workspaceSubmissionMetricBuilder(workspaceName: WorkspaceName, submissionId: UUID): ExpandedMetricBuilder =
    workspaceMetricBuilder(workspaceName).expand(SubmissionMetric, submissionId)

  /**
    * Provides a counter for a SubmissionStatus.
    * @param builder base builder used to generate the counter
    * @return SubmissionStatus => Counter
    */
  protected def submissionStatusCounter(builder: ExpandedMetricBuilder): SubmissionStatus => Counter =
    status => builder
      .expand(SubmissionStatusMetric, status)
      .asCounter()

  /**
    * Provides a counter for a WorkflowStatus.
    * @param builder base builder used to generate the counter
    * @return WorkflowStatus => Counter
    */
  protected def workflowStatusCounter(builder: ExpandedMetricBuilder): WorkflowStatus => Counter =
    status => builder
      .expand(WorkflowStatusMetric, status)
      .asCounter()

  protected def httpRequestCounter(builder: ExpandedMetricBuilder): HttpRequest => Counter =
    httpRequest => builder
      .expand("request", httpRequest.uri)
      .asCounter()

//  protected def httpRequestCounter2: HttpRequest => Counter =
//    httpRequest => ExpandedMetricBuilder.expand("request").expand(httpRequest.method).expand(httpRequest.uri).asCounter()

  protected def httpResponseCounter(builder: ExpandedMetricBuilder): HttpResponse => Counter =
    httpResponse => builder.expand("response").expand(httpResponse.status).asCounter()

  protected def httpRequestTimer(builder: ExpandedMetricBuilder): HttpRequest => Timer =
    httpRequest => builder.expand("latency").expand(httpRequest.method).expand(httpRequest.uri).asTimer()

  protected def httpRequestTimer2: HttpRequest => Timer =
    httpRequest => ExpandedMetricBuilder.expand("latency").expand(httpRequest.method).expand(httpRequest.uri).asTimer()
}

object RawlsInstrumented {
  /**
    * Adds a .countDBResult method to Counter which counts the result of a numeric DBIOAction.
    */
  implicit class CounterDBIOActionSupport(counter: Counter) {
    def countDBResult[R, S <: NoStream, E <: Effect](action: DBIOAction[R, S, E])(implicit numeric: Numeric[R], executionContext: ExecutionContext): DBIOAction[R, NoStream, E] =
      action.map { count =>
        counter += numeric.toLong(count)
        count
      }
  }
}

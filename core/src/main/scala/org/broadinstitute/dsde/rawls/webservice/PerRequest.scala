package org.broadinstitute.dsde.rawls.webservice

import scala.language.postfixOps
import akka.http.scaladsl.marshalling.{Marshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{HttpHeader, StatusCodes, Uri}
import akka.http.scaladsl.model.headers.Location
import org.broadinstitute.dsde.rawls.RawlsException
import org.broadinstitute.dsde.rawls.model.ErrorReport

import scala.concurrent.ExecutionContext

object PerRequest {

  import akka.http.scaladsl.marshalling.PredefinedToResponseMarshallers._

  implicit def requestCompleteMarshaller(implicit executionContext: ExecutionContext): ToResponseMarshaller[PerRequestMessage] = Marshaller {
    _: ExecutionContext => {
      case requestComplete@ RequestComplete(errorReport: ErrorReport) =>
        requestComplete.marshaller(requestComplete.response).map(_.map(_.map(_.copy(status = errorReport.statusCode.getOrElse(StatusCodes.InternalServerError)))))
      case requestComplete: RequestComplete[_] =>
        requestComplete.marshaller(requestComplete.response)

      case requestComplete@ RequestCompleteWithHeaders(errorReport: ErrorReport, _) =>
        requestComplete.marshaller(requestComplete.response).map(_.map(_.map(_.mapHeaders(_ ++ requestComplete.headers).copy(status = errorReport.statusCode.getOrElse(StatusCodes.InternalServerError)))))
      case requestComplete: RequestCompleteWithHeaders[_] =>
        requestComplete.marshaller(requestComplete.response).map(_.map(_.map(_.mapHeaders(_ ++ requestComplete.headers))))
    }
  }

  sealed trait PerRequestMessage
  /**
   * Report complete, follows same pattern as spray.routing.RequestContext.complete; examples of how to call
   * that method should apply here too. E.g. even though this method has only one parameter, it can be called
   * with 2 where the first is a StatusCode: RequestComplete(StatusCode.Created, response)
   */
  case class RequestComplete[T](response: T)(implicit val marshaller: ToResponseMarshaller[T]) extends PerRequestMessage

  /**
   * Report complete with response headers. To response with a special status code the first parameter can be a
   * tuple where the first element is StatusCode: RequestCompleteWithHeaders((StatusCode.Created, results), header).
   * Note that this is here so that RequestComplete above can behave like spray.routing.RequestContext.complete.
   */
  case class RequestCompleteWithHeaders[T](response: T, headers: HttpHeader*)(implicit val marshaller: ToResponseMarshaller[T]) extends PerRequestMessage
}

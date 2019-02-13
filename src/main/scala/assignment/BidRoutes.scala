package assignment

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.Timeout
import assignment.BiddingAgent.RequestBid

import scala.concurrent.Future
import scala.concurrent.duration._
import assignment.BidRequestProtocol._
import assignment.BidResponseProtocol._
import akka.http.scaladsl.server.Directives

trait BidRoutes extends Directives with JsonSupport {

  implicit def system: ActorSystem
  lazy val log = Logging(system, classOf[BidRoutes])
  def biddingAgent: ActorRef
  implicit lazy val timeout = Timeout(60.seconds)
  lazy val bidRoutes: Route =
    path("bid") {
//      concat (
//        get {
//          val request: Future[BidRequest] = (biddingAgent ? Test).mapTo[BidRequest]
//          complete(request)
//        },
        post {
          entity(as[BidRequest]) { request =>
            val bidResponse: Future[Option[BidResponse]] = (biddingAgent ? RequestBid(request)).mapTo[Option[BidResponse]]

            onSuccess(bidResponse) { response =>
              response match {
                case Some(resp) => complete(resp)
                case None => complete(StatusCodes.NoContent)
              }
            }
          }
        }
//      )
    }
}

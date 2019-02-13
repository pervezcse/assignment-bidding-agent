package assignment

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import assignment.BiddingAgent.LoadCampaigns
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object HttpServer extends App with BidRoutes {

  implicit val system: ActorSystem = ActorSystem("httpServer")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  val biddingAgent: ActorRef = system.actorOf(BiddingAgent.props, "biddingAgent")
  biddingAgent ! LoadCampaigns
  lazy val routes: Route = bidRoutes
  Http().bindAndHandle(routes, "localhost", 8080)
  println(s"Server online at http://localhost:8080/")
  Await.result(system.whenTerminated, Duration.Inf)
}
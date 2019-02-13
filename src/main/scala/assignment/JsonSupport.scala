package assignment

//#json-support
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import BidRequestProtocol._
import BidResponseProtocol._
import assignment.CampaignProtocol.Banner

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val geoJsonFormat = jsonFormat4(Geo)
  implicit val deviceJsonFormat = jsonFormat2(Device)
  implicit val userJsonFormat = jsonFormat2(User)
  implicit val siteJsonFormat = jsonFormat2(Site)
  implicit val impressionJsonFormat = jsonFormat8(Impression)
  implicit val bidRequestJsonFormat = jsonFormat5(BidRequest)
  implicit val bannerJsonFormat = jsonFormat4(Banner)
  implicit val bidResponseJsonFormat = jsonFormat5(BidResponse)
}
//#json-support

package assignment

import akka.actor.{Actor, Props}
import assignment.CampaignProtocol.DeviceType.DeviceType
import assignment.CampaignProtocol.UserType.UserType

import scala.collection.immutable.HashSet
import scala.collection.immutable.Vector

object BidRequestProtocol {
  final case class BidRequest(id: String, imp: Option[List[Impression]], site: Site, user: Option[User], device: Option[Device])
  final case class Impression(id: String, wmin: Option[Int], wmax: Option[Int], w: Option[Int], hmin: Option[Int], hmax: Option[Int], h: Option[Int], bidFloor: Option[Double])
  final case class Site(id: Int, domain: String)

  // User and Device can be super-classed?
  final case class User(id: String, geo: Option[Geo])
  final case class Device(id: String, geo: Option[Geo])

  final case class Geo(country: Option[String], city: Option[String], lat: Option[Double], lon: Option[Double])
}

object BidResponseProtocol {
  import CampaignProtocol.Banner
  final case class BidResponse(id: String, bidRequestId: String, price: Double, adid: Option[String], banner: Option[Banner])
}

object CampaignProtocol {
  final case class Campaign(id: Int, userId: Int, country: String, runningTimes: Set[TimeRange], targeting: Targeting, banners: List[Banner], bid: Double)
  final case class TimeRange(timeStart: Long, timeEnd: Long)

  // I choose HashSet for fast lookup since targetedSiteIds can be potentially long list.
  // Targeting can be done based on new or returning users as well as types of devices they are using
  final case class Targeting(cities: List[String], targetedSiteIds: HashSet[Int], userType: Option[UserType], deviceType: Option[DeviceType])

  final case object DeviceType extends Enumeration {
    type DeviceType = Value
    val MOBILE, TAB, DESKTOP = Value
  }

  final case object UserType extends Enumeration {
    type UserType = Value
    val NEW, RETURNING = Value
  }

  final case class Banner(id: Int, src: String, width: Int, height: Int)
}

object BiddingAgent {
  import BidRequestProtocol._

  final case class RequestBid(request: BidRequest)
  final case object LoadCampaigns

//  final case object Test;

  def props: Props = Props[BiddingAgent]
}

class BiddingAgent extends Actor {
  import BidRequestProtocol._
  import BidResponseProtocol._
  import CampaignProtocol._
  import BiddingAgent._

  var campaigns = Vector.empty[Campaign]

  override def receive: Receive = {

    case RequestBid(request) => {

      def matchSite(campaign: Campaign, site: Site): Boolean = campaign.targeting.targetedSiteIds.contains(request.site.id)

      def matchUser(campaign: Campaign, request: BidRequest): Boolean = {
        val matchCountry = for {
          user <- request.user
          geo <- user.geo
          country <- geo.country
          mc = country.toUpperCase==campaign.country
        } yield (mc)
        val matchCity = for {
          user <- request.user
          geo <- user.geo
          city <- geo.city
          mc = campaign.targeting.cities.isEmpty || campaign.targeting.cities.contains(city.toUpperCase)
        } yield (mc)

        if(matchCountry.isEmpty)
          return !campaign.targeting.cities.isEmpty && matchCity.getOrElse(false)
        else
            return matchCountry.get && (campaign.targeting.cities.isEmpty || matchCity.getOrElse(false))
      }

      def matchRunningTime(campaign: Campaign): Boolean = {
        val currentTime = System.currentTimeMillis()
        return campaign.runningTimes.exists(rt => rt.timeStart<=currentTime && rt.timeEnd>=currentTime)
      }

      def matchDevice(campaign: Campaign, request: BidRequest): Boolean = {
        val matchCountry = for {
          device <- request.device
          geo <- device.geo
          country <- geo.country
          mc = country.toUpperCase==campaign.country
        } yield (mc)
        val matchCity = for {
          device <- request.device
          geo <- device.geo
          city <- geo.city
          mc = campaign.targeting.cities.isEmpty || campaign.targeting.cities.contains(city.toUpperCase)
        } yield (mc)

        if(matchCountry.isEmpty)
          return !campaign.targeting.cities.isEmpty && matchCity.getOrElse(false)
        else
          return matchCountry.get && (campaign.targeting.cities.isEmpty || matchCity.getOrElse(false))
      }

      def matchBannerSize(imp: Impression, banner: Banner): Boolean = {

        def matchSize(x: Option[Int], xMin: Option[Int], xMax: Option[Int], xToMatch: Int): Boolean = (x, xMin, xMax) match {
          case (Some(size), _, _) => size==xToMatch
          case (None, Some(min), Some(max)) => min<=xToMatch && xToMatch<=max
          case (None, Some(min), None) => min<=xToMatch
          case (None, None, Some(max)) => max>=xToMatch
          case _ => false
        }
        val heightMatch = matchSize(imp.h, imp.hmin, imp.hmax, banner.height)
        val widthMatch = matchSize(imp.w, imp.wmin, imp.wmax, banner.width)
        return heightMatch && widthMatch
      }

      def matchImpressions(campaign: Campaign, request: BidRequest): Option[Banner] = {
        if(!campaign.banners.isEmpty) {
          for (impList <- request.imp) {
            for (imp <- impList) {
              if (campaign.bid >= imp.bidFloor.getOrElse(0.0)) {
                val banner = campaign.banners.find(matchBannerSize(imp, _))
                if (!banner.isEmpty)
                  return banner
              }
            }
            return None
          }
          return Some(campaign.banners(0))
        }
        return None
      }

      var winningCampaign: Option[Campaign] = None
      var winningBanner: Option[Banner] = None
      for(campaign <- campaigns if winningCampaign.isEmpty) {
        if (matchRunningTime(campaign) && matchSite(campaign, request.site)
          && (matchUser(campaign, request) || matchDevice(campaign, request))) {
          val banner = matchImpressions(campaign, request)
          if(!banner.isEmpty) {
            winningCampaign = Some(campaign)
            winningBanner = banner
          }
        }
      }
      val bidResponse = winningCampaign match {
        case Some(campaign) => Some(BidResponse("1", request.id, campaign.bid, Some("adid"), winningBanner))
        case None => None
      }
      sender() ! bidResponse
    }

    case LoadCampaigns => {
      val robiBanners = List(Banner(1, "robi", 320, 50), Banner(2, "robi", 320, 480), Banner(3, "robi", 320, 250))
      val robiTargeting = Targeting(List("DHAKA", "CHITTAGONG", "KHULNA"), HashSet(1, 2, 3), None, None)
      val robiRunningTimes = Set(TimeRange(1527497747563L, 1537497747563L), TimeRange(152749770000L, 1537497740000L))
      val robiCampaign = Campaign(1, 1, "BANGLADESH", robiRunningTimes, robiTargeting, robiBanners, 10.0)

      val airtelBanners = List(Banner(1, "airtel", 320, 50), Banner(2, "airtel", 320, 480), Banner(3, "airtel", 320, 250))
      val airtelTargeting = Targeting(List("RAJSHAHI", "BOGURA", "DHAKA"), HashSet(4, 5, 6), None, None)
      val airtelRunningTimes = Set(TimeRange(1527497747563L, 1537497747563L), TimeRange(152749770000L, 1537497740000L))
      val airtelCampaign = Campaign(1, 1, "BANGLADESH", airtelRunningTimes, airtelTargeting, airtelBanners, 20.0)

      val cocacolaBanners = List(Banner(1, "cocacola", 320, 50), Banner(2, "cocacola", 320, 480), Banner(3, "cocacola", 320, 250))
      val cocacolaTargeting = Targeting(List("KOLKATA", "DELHI"), HashSet(1, 2, 3), None, None)
      val cocacolaRunningTimes = Set(TimeRange(1527497747563L, 1537497747563L), TimeRange(152749770000L, 1537497740000L))
      val cocacolaCampaign = Campaign(1, 1, "INDIA", cocacolaRunningTimes, cocacolaTargeting, cocacolaBanners, 30.0)

      campaigns = Vector(robiCampaign, airtelCampaign, cocacolaCampaign).sortWith(_.bid > _.bid)
    }

//    case Test => {
//      import BidRequestProtocol._
//
//      val geo = Geo(Some("Bangladesh"), Some("Dhaka"), Some(-25.744241), Some(28.189829))
//      val device = Device("1", Some(geo))
//      val user = User("1", Some(geo))
//      val site = Site(1, "www.google.com")
//      val imp1 = Impression("1", None, None, Some(320), None, None, Some(50), Some(5.0))
//      val imp2 = Impression("2", None, None, Some(320), None, None, Some(480), Some(5.0))
//      val request = BidRequest("1", Some(List(imp1, imp2)), site, Some(user), Some(device))
//
//      sender() ! request
//    }
  }
}
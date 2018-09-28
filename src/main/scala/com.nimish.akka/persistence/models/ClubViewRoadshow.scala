package com.nimish.akka.persistence.models

import com.nimish.akka.Constants.{EMPTY, MANNED_YES, MANNED_NO}
import org.joda.time.{DateTime, LocalDate}

case class ClubDownloadRoadshow(
   roadshowNumber: Int
   , roadshowDescription: String
   , vendorNumber: Int
   , vendorName: String
   , eventDesc: String
   , status: String
   , clubInstructions: String
   , startDate: LocalDate
   , endDate: LocalDate
   , createdAt: DateTime
   , itemNumbers: Seq[Option[Int]]
   , palletsRequired: Int
   , clubName: Option[String] = None
   , market: Option[Int] = None
   , clubNumber: Option[Long] = None
   , returnAddress: Option[ReturnAddress]
   , manned: String)

object ClubDownloadRoadshow {
  def create(roadshow : ((RoadshowBuyer, RoadshowClub, Option[ReturnAddress]), Seq[Option[RoadshowItem]]), vdr: Map[Int, String]) =
    ClubDownloadRoadshow(
      roadshow._1._1.roadshowNumber
      , roadshow._1._1.roadshowDescription
      , roadshow._1._1.vendorNumber
      , vdr.get(roadshow._1._1.vendorNumber) match {
        case Some(t) => t
        case None => EMPTY
      }
      , roadshow._1._1.eventDesc
      , roadshow._1._2.status
      , roadshow._1._2.clubInstructions
      , roadshow._1._2.startDate
      , roadshow._1._2.endDate
      , DateTime.now()
      , roadshow._2.map( i => i.map(item => item.itemNbr))
      , roadshow._1._2.palletsRequired
      , Some(roadshow._1._2.clubName)
      , Some(roadshow._1._2.market)
      , Some(roadshow._1._2.clubNumber)
      , roadshow._1._3
      , roadshow._1._1.manned match {
        case 1 => MANNED_YES
        case 0 => MANNED_NO
      }
    )
}

case class ClubViewDownload(clubNumber: Long, clubName: String, market: Int, roadshows: Seq[ClubDownloadRoadshow])

object ClubViewDownload {
  def create(all: ((Long, String, Int), Seq[ClubDownloadRoadshow])) =
    ClubViewDownload(
      all._1._1
      , all._1._2
      , all._1._3
      , all._2
    )
}

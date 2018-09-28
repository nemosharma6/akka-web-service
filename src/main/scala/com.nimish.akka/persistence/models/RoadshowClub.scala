package com.nimish.akka.persistence.models

import org.joda.time.LocalDate

case class RoadshowClub(
   roadshowNumber: Int
  , clubNumber: Long
  , clubName: String
  , market: Int
  , clubInstructions: String
  , status: String
  , startDate: LocalDate
  , endDate: LocalDate
  , palletsRequired: Int)

case class RoadshowClubForEdit(
   roadshowNumber: Int
  , clubNumber: Long
  , clubName: String
  , market: Int
  , clubInstructions: String
  , status: String
  , startDate: LocalDate
  , endDate: LocalDate
  , palletsRequired: Int
  , palletCapacity: Int)

object RoadshowClubForEdit {
  def create(cl: RoadshowClub, mp: Map[Int, ClubInfo]) =
    RoadshowClubForEdit(
      cl.roadshowNumber
      , cl.clubNumber
      , cl.clubName
      , cl.market
      , cl.clubInstructions
      , cl.status
      , cl.startDate
      , cl.endDate
      , cl.palletsRequired
      , mp(cl.clubNumber.toInt).palletCapacity
    )
}

case class RoadshowClubBuyer(
  roadshowNumber: Int
  , clubNumber: Long
  , clubName: String
  , market: Int
  , status: String
  , startDate: LocalDate
  , endDate: LocalDate
  , palletsRequired: Int)

case class RoadshowClubWithFlagBuyer(
  roadshowNumber: Int
  , clubNumber: Long
  , clubName: String
  , market: Int
  , status: String
  , startDate: LocalDate
  , endDate: LocalDate
  , palletsRequired: Int
  , palletsUsed: Option[Int])

object RoadshowClubWithFlagBuyer {
  def create(club: RoadshowClubBuyer, palletsUsed: Option[Int] = None) =
    RoadshowClubWithFlagBuyer(
      club.roadshowNumber
      , club.clubNumber
      , club.clubName
      , club.market
      , club.status
      , club.startDate
      , club.endDate
      , club.palletsRequired
      , palletsUsed
    )
}

case class RoadshowClubWithConflict(
  roadshowNumber: Int
  , clubNumber: Long
  , clubName: String
  , market: Int
  , status: String
  , startDate: LocalDate
  , endDate: LocalDate
  , palletsRequired: Int
  , conflicts: Option[Seq[String]])

object RoadshowClubWithConflict {
  def create(cl: RoadshowClubBuyer, cf: Option[Seq[String]]) =
    RoadshowClubWithConflict(
      cl.roadshowNumber
      , cl.clubNumber
      , cl.clubName
      , cl.market
      , cl.status
      , cl.startDate
      , cl.endDate
      , cl.palletsRequired
      , cf
    )
}

case class RoadshowClubWithFlagBuyerTemp(
  roadshowNumber: Int
  , clubNumber: Long
  , clubName: String
  , market: Int
  , status: String
  , startDate: LocalDate
  , endDate: LocalDate
  , palletsRequired: Int
  , palletsUsed: Option[Int]
  , conflicts: Option[Seq[String]])

object RoadshowClubWithFlagBuyerTemp {
  def create(cl: RoadshowClubWithConflict, palletsUsed: Option[Int] = None) =
    RoadshowClubWithFlagBuyerTemp(
      cl.roadshowNumber
      , cl.clubNumber
      , cl.clubName
      , cl.market
      , cl.status
      , cl.startDate
      , cl.endDate
      , cl.palletsRequired
      , palletsUsed
      , cl.conflicts
    )
}

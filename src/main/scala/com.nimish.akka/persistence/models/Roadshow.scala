package com.nimish.akka.persistence.models

import org.joda.time.{DateTime, LocalDate}
import com.nimish.akka.Constants.{EMPTY, MANNED_YES, MANNED_NO}

case class Roadshow(
   roadshowNumber: Int
   , roadshowDescription: String
   , vendorNumber: Int
   , eventCode: Int
   , eventDesc: String
   , startDate: LocalDate
   , endDate: LocalDate
   , returnAddressId: Option[Int]
   , manned: Int
   , createdAt: DateTime
   , lastModifiedAt: DateTime)

case class RoadshowBuyer(
   roadshowNumber: Int
   , roadshowDescription: String
   , vendorNumber: Int
   , eventCode: Int
   , eventDesc: String
   , manned: Int)

case class RoadshowMetaResponse(
   roadshowNumber: Int
   , roadshowDescription: String
   , vendorName: String
   , eventCode: Int
   , eventDesc: String
   , status: String
   , startDate: LocalDate
   , endDate: LocalDate)

object RoadshowMetaResponse {
  def create(roadshow: (RoadshowBuyer, RoadshowClubBuyer), vdr: Map[Int, String]) =
    new RoadshowMetaResponse(
      roadshow._1.roadshowNumber
      , roadshow._1.roadshowDescription
      , vdr.get(roadshow._1.vendorNumber) match {
        case Some(t) => t
        case None => EMPTY
      }
      , roadshow._1.eventCode
      , roadshow._1.eventDesc
      , roadshow._2.status
      , roadshow._2.startDate
      , roadshow._2.endDate
    )
  }

case class RoadshowBuyerView(
  roadshowNumber: Int
  , roadshowDescription: String
  , vendorNumber: Int
  , vendorName: String
  , eventCode: Int
  , eventDesc: String
  , club: RoadshowClubWithFlagBuyer
  , manned: String)

object RoadshowBuyerView{
  def create(roadshow: (RoadshowBuyer, RoadshowClubBuyer, Option[Int]), vendor: Map[Int,String]) = {
    RoadshowBuyerView(
      roadshow._1.roadshowNumber
      , roadshow._1.roadshowDescription
      , roadshow._1.vendorNumber
      , vendor.get(roadshow._1.vendorNumber) match {
        case Some(t) => t
        case None => EMPTY
      }
      , roadshow._1.eventCode
      , roadshow._1.eventDesc
      , RoadshowClubWithFlagBuyer.create(roadshow._2, roadshow._3)
      , roadshow._1.manned match {
        case 1 => MANNED_YES
        case 0 => MANNED_NO
      }
    )
  }
}

case class RoadshowBuyerViewTemp(
  roadshowNumber: Int
  , roadshowDescription: String
  , vendorNumber: Int
  , vendorName: String
  , eventCode: Int
  , eventDesc: String
  , club: RoadshowClubWithFlagBuyerTemp)

object RoadshowBuyerViewTemp {
  def create(roadshow: (RoadshowBuyer, RoadshowClubWithConflict, Option[Int]), vendor: Map[Int,String]) = {
    new RoadshowBuyerViewTemp(
      roadshow._1.roadshowNumber
      , roadshow._1.roadshowDescription
      , roadshow._1.vendorNumber
      , vendor.get(roadshow._1.vendorNumber) match {
        case Some(t) => t
        case None => {
          "EMPTY"
        }
      }
      , roadshow._1.eventCode
      , roadshow._1.eventDesc
      , RoadshowClubWithFlagBuyerTemp.create(roadshow._2, roadshow._3)
    )
  }
}

case class RoadshowWithItem(
  roadshowNumber: Int
  , roadshowDescription: String
  , vendorNumber: Int
  , vendorName: String
  , eventCode: Int
  , eventDesc: String
  , items: Seq[Option[RoadshowItem]]
  , club: RoadshowClubBuyer
  , returnAddress: Option[ReturnAddress])

object RoadshowWithItem {
  def create(roadshow: ((RoadshowBuyer, RoadshowClubBuyer, Option[ReturnAddress]), Seq[Option[RoadshowItem]]), vdr: Map[Int,String]) =
    RoadshowWithItem(
      roadshow._1._1.roadshowNumber
      , roadshow._1._1.roadshowDescription
      , roadshow._1._1.vendorNumber
      , vdr.get(roadshow._1._1.vendorNumber) match {
        case Some(t) => t
        case None => EMPTY
      }
      , roadshow._1._1.eventCode
      , roadshow._1._1.eventDesc
      , roadshow._2 match {
        case Seq(None) => Seq()
        case it => it
      }
      , roadshow._1._2
      , roadshow._1._3
    )
}

case class RoadshowEdit(
  roadshowNumber: Int
  , roadshowDescription: String
  , vendorNumber: Int
  , vendorName: String
  , eventCode: Int
  , eventDesc: String
  , items: Seq[Option[RoadshowItem]]
  , clubs: Seq[RoadshowClubForEdit]
  , returnAddress: Option[ReturnAddress]
  , manned: Boolean)

object RoadshowEdit {
  def create(roadshow: (RoadshowBuyer, Seq[RoadshowClub], Seq[Option[RoadshowItem]], Option[ReturnAddress]), vdr: Map[Int,String], cl: Seq[RoadshowClubForEdit]) =
    RoadshowEdit(
      roadshow._1.roadshowNumber
      , roadshow._1.roadshowDescription
      , roadshow._1.vendorNumber
      , vdr.get(roadshow._1.vendorNumber) match {
        case Some(t) => t
        case None => EMPTY
      }
      , roadshow._1.eventCode
      , roadshow._1.eventDesc
      , roadshow._3 match {
        case Seq(None) => Seq()
        case it => it
      }
      , cl
      , roadshow._4
      , roadshow._1.manned match {
        case 1 => true
        case 0 => false
      })
}

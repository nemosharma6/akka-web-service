package com.nimish.akka.persistence.models

import com.nimish.akka.Constants.NINETY_DAY
import org.joda.time.LocalDate

case class RoadshowFilterRequest(
  vendorNumber: Option[Int] = None
  , roadshowNumber: Option[Seq[Int]] = None
  , clubNumber: Option[Seq[Long]] = None
  , status: Option[String] = None
  , startDate: LocalDate
  , endDate: LocalDate)

case class GenericFilterRequest(
  vendorNumber: Option[Int] = None
  , roadshowNumber: Option[Seq[Int]] = None
  , clubNumber: Option[Seq[Long]] = None
  , status: Option[String] = None
  , startDate: Option[LocalDate] = None
  , endDate: Option[LocalDate] = None
  )

object GenericFilterRequest {
  def create(request: RoadshowFilterRequest, status: Option[String] = None) =
    GenericFilterRequest(
      request.vendorNumber
      , request.roadshowNumber
      , request.clubNumber
      , status
      , Some(request.startDate)
      , Some(request.endDate)
    )

  def createBuyerRequest(request: RoadshowFilterRequest) =
    GenericFilterRequest(
      request.vendorNumber
      , request.roadshowNumber
      , request.clubNumber
      , request.status
      , Some(request.startDate.minusDays(NINETY_DAY))
      , Some(request.endDate)
    )
}

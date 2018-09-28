package com.nimish.akka.persistence.models

import org.joda.time.LocalDate

case class ClubsPalletRequest(clubs: Seq[Long], startDate: LocalDate, endDate: LocalDate)
case class PalletResponse(clubNbr: Long, dates: Seq[PalletCount])
case class PalletCount(date: LocalDate, count: Int)

case class PalletReport(
  clubNbr: Long
  , city: Option[String]
  , stateCode: Option[String]
  , regionNbr: Int
  , districtNbr: Int
  , capacity: Int
  , dates: Seq[PalletCount])

case class PalletResponseWithCapacity(clubNbr: Long, palletCapacity: Int, dates: Seq[PalletCount])

object PalletResponseWithCapacity {
  def create(plt: PalletResponse, club: Map[Int, ClubInfo]) =
    PalletResponseWithCapacity(
      plt.clubNbr
      , club.get(plt.clubNbr.toInt) match {
        case Some(cp) => cp.palletCapacity
        case None => -1
      }
      , plt.dates
    )
}

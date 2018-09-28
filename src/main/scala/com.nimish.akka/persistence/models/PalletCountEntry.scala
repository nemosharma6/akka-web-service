package com.nimish.akka.persistence.models

import org.joda.time.LocalDate

case class PalletCountEntry(clubNumber: Long, date: LocalDate, palletUsed: Int)
case class PalletCountList(clubNumber: Long, startDate: LocalDate, endDate: LocalDate, palletUsed: Option[Int])

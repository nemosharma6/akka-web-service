package com.nimish.akka.persistence.models

case class ClubInfo(
   clubNumber: Int
   , city: Option[String]
   , stateCode: Option[String]
   , regionNbr: Int
   , districtNbr: Int
   , palletsAvailable: Int
   , palletCapacity: Int)

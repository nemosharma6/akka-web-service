package com.nimish.akka.persistence.models

import com.nimish.akka.mappings.JsonMappings
import com.nimish.akka.utils.{ActorsConfig, Config}
import org.joda.time.{DateTime, LocalDate}

case class AuditEntry(byUserId: String, byUserName: String, at: DateTime)

case class Restriction(
  id: Int
  , name: String
  , clubs: Option[Seq[Long]]
  , items: Option[Seq[Int]]
  , subCategory: Option[Seq[Int]]
  , vendorNumber: Option[Seq[Int]]
  , startDate: LocalDate
  , endDate: Option[LocalDate]
  , noEndFlag: Int
  , message: String
  , indicator: Char
  , created: AuditEntry
  , modified: AuditEntry)

case class RestrictionData(
  id: Int
  , lastModifiedAt: DateTime
  , club: Option[Long]
  , item: Option[Int]
  , subCategory: Option[Int]
  , vendorNumber: Option[Int])

case class RestrictionEntry(
  id: Int
  , name: String
  , startDate: LocalDate
  , endDate: Option[LocalDate]
  , noEndFlag: Int
  , message: String
  , indicator: Char
  , created: AuditEntry
  , modified: AuditEntry)

object Restriction extends Config with ActorsConfig with JsonMappings {
  def create(restriction: RestrictionEntry, restrictionData: Seq[RestrictionData], mp: Map[String, String]) =
    Restriction(
      restriction.id
      , restriction.name
      , {
        val cl = restrictionData.flatMap(_.club).distinct
        cl.length match {
          case 0 => None
          case _ => Some(cl)
        }
      }
      , {
        val it = restrictionData.flatMap(_.item).distinct
        it.length match {
          case 0 => None
          case _ => Some(it)
        }
      }
      , {
        val subCat = restrictionData.flatMap(_.subCategory).distinct
        subCat.length match {
          case 0 => None
          case _ => Some(subCat)
        }
      }
      , {
        val vdr = restrictionData.flatMap(_.vendorNumber).distinct
        vdr.length match {
          case 0 => None
          case _ => Some(vdr)
        }
      }
      , restriction.startDate
      , restriction.endDate
      , restriction.noEndFlag
      , restriction.message
      , restriction.indicator
      , AuditEntry(mp(restriction.created.byUserId), mp(restriction.created.byUserName), restriction.created.at)
      , AuditEntry(mp(restriction.modified.byUserId), mp(restriction.modified.byUserName), restriction.modified.at)
    )
}

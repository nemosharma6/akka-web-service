package com.nimish.akka.persistence.models

import org.joda.time.{DateTime, LocalDate}

case class Conflicts(
  id: Int
  , name: String
  , conflictType: String
  , subCategory: Option[Seq[Int]]
  , field: Option[String]
  , startDate: LocalDate
  , endDate: Option[LocalDate]
  , timeFrame: Int
  , indicator: Char
  , created: AuditEntry
  , modified: AuditEntry)

case class ConflictEntry(
  id: Int
  , name: String
  , conflictType: String
  , startDate: LocalDate
  , endDate: Option[LocalDate]
  , noEndFlag: Int
  , timeFrame: Int
  , status: Char
  , indicator: Char
  , created: AuditEntry
  , modified: AuditEntry)

case class ConflictData(id: Int, lastModifiedAt: DateTime, subCategory: Option[Int], field: Option[String])

object Conflicts {
  def create(conflict: ConflictEntry, data: Seq[ConflictData], mp: Map[String, String]) =
    Conflicts(
      conflict.id
      , conflict.name
      , conflict.conflictType
      , {
        val subCat = data.flatMap(_.subCategory).distinct
        subCat.length match {
          case 0 => None
          case _ => Some(subCat)
        }
      }
      , data.map(_.field).head
      , conflict.startDate
      , conflict.endDate
      , conflict.timeFrame
      , conflict.indicator
      , AuditEntry(mp(conflict.created.byUserId), mp(conflict.created.byUserName), conflict.created.at)
      , AuditEntry(mp(conflict.modified.byUserId), mp(conflict.modified.byUserName), conflict.modified.at)
    )

  def tempCreate(conflict: (ConflictEntry, Seq[ConflictData])) =
    Conflicts(
      conflict._1.id
      , conflict._1.name
      , conflict._1.conflictType
      , {
        val subCat = conflict._2.flatMap(_.subCategory).distinct
        subCat.length match {
          case 0 => None
          case _ => Some(subCat)
        }
      }
      , conflict._2.map(_.field).head
      , conflict._1.startDate
      , conflict._1.endDate
      , conflict._1.timeFrame
      , conflict._1.indicator
      , conflict._1.created
      , conflict._1.modified
    )
}

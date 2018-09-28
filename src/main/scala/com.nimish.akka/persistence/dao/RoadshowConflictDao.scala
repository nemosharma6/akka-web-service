package com.nimish.akka.persistence.dao

import com.nimish.akka.persistence.models._
import com.nimish.akka.utils.ActorsConfig
import org.joda.time.{DateTime, DateTimeZone, LocalDate}
import com.nimish.akka.Constants.ACTIVE
import slick.jdbc.JdbcProfile

class RoadshowConflictDao(val profile: JdbcProfile) extends ActorsConfig {

  import profile.api._

  implicit val utilDate2SqlDate = MappedColumnType.base[DateTime, java.sql.Timestamp](
    utilDate => new java.sql.Timestamp(utilDate.getMillis),
    sqlDate => new DateTime(sqlDate.getTime)
  )

  implicit val utilLocalDate2SqlDate = MappedColumnType.base[LocalDate, java.sql.Date](
    utilDate => new java.sql.Date(utilDate.toDateTimeAtStartOfDay(DateTimeZone.UTC).getMillis),
    sqlDate => new LocalDate(sqlDate.getTime, DateTimeZone.UTC)
  )

  class ConflictEntryTable(tag: Tag) extends Table[ConflictEntry](tag, "rs_conflicts") {
    def created = (createdById, createdByName, createdAt) <> (AuditEntry.tupled, AuditEntry.unapply)

    def modified = (lastModifiedById, lastModifiedByName, lastModifiedAt) <> (AuditEntry.tupled, AuditEntry.unapply)

    def * = (id, name, conflictType, startDate, endDate, noEndFlag, timeFrame, status, indicator, created,
      modified) <> ((ConflictEntry.apply _).tupled, ConflictEntry.unapply)

    def id = column[Int]("id")

    def name = column[String]("name")

    def conflictType = column[String]("conflict_type")

    def startDate = column[LocalDate]("start_date")

    def endDate = column[Option[LocalDate]]("end_date")

    def noEndFlag = column[Int]("no_end_flag")

    def timeFrame = column[Int]("time_frame")

    def status = column[Char]("status")

    def indicator = column[Char]("indicator")

    def createdAt = column[DateTime]("created_at")

    def createdById = column[String]("created_by_id")

    def createdByName = column[String]("created_by_name")

    def lastModifiedAt = column[DateTime]("last_modified_at")

    def lastModifiedById = column[String]("last_modified_by_id")

    def lastModifiedByName = column[String]("last_modified_by_name")
  }

  class ConflictDataTable(tag: Tag) extends Table[ConflictData](tag, "rs_conflicts_data") {

    def * = (id, lastModifiedAt, subCategory, field) <> ((ConflictData.apply _).tupled, ConflictData.unapply)

    def id = column[Int]("id")

    def lastModifiedAt = column[DateTime]("last_modified_at")

    def subCategory = column[Option[Int]]("sub_category")

    def field = column[Option[String]]("field")
  }

  val conflictDataTableQuery = TableQuery[ConflictDataTable]
  val conflictEntryTableQuery = TableQuery[ConflictEntryTable]

  def fetchForId(id: Int): DBIO[Seq[ConflictEntry]] = {
    conflictEntryTableQuery.filter(_.id === id).result
  }

  def fetchConflictEntry: DBIO[Seq[ConflictEntry]] = conflictEntryTableQuery.filter(_.indicator === ACTIVE).result

  def fetchConflictData(list: Seq[(Int, DateTime)]): DBIO[Seq[ConflictData]] = {
    conflictDataTableQuery.filter(row => list.map(e => row.id === e._1 && row.lastModifiedAt === e._2).reduceLeft(_ || _)).result
  }

  def fetchConflictDataLogForId(id: Int): DBIO[Map[(Int, DateTime), Seq[ConflictData]]] =
    conflictDataTableQuery.filter(_.id === id).result.map {
      _.groupBy(row => (row.id, row.lastModifiedAt)).map {
        case (k, v) => Map(k -> v)
      }.reduceLeft(_ ++ _)
    }

  def fetch: DBIO[Seq[(ConflictEntry, Seq[ConflictData])]] = {
    (conflictEntryTableQuery.filter(_.indicator === ACTIVE) join conflictDataTableQuery on ((en, dt) => en.id === dt.id && en.lastModifiedAt === dt.lastModifiedAt)).result.map {
      _.groupBy(_._1).map {
        case (k, v) => (k, v.map(_._2))
      }.toSeq
    }
  }
}

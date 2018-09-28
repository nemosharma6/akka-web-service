package com.nimish.akka.persistence.dao

import com.nimish.akka.Constants.ACTIVE
import com.nimish.akka.persistence.models.{AuditEntry, RestrictionData, RestrictionEntry}
import com.nimish.akka.utils.{ActorsConfig, Config}
import org.joda.time.{DateTime, DateTimeZone, LocalDate}
import slick.jdbc.JdbcProfile

class RoadshowRestrictionDao(val profile: JdbcProfile) extends Config with ActorsConfig{

  import profile.api._


  implicit val utilDate2SqlDate = MappedColumnType.base[DateTime, java.sql.Timestamp](
    utilDate => new java.sql.Timestamp(utilDate.getMillis),
    sqlDate => new DateTime(sqlDate.getTime)
  )

  implicit val utilLocalDate2SqlDate = MappedColumnType.base[LocalDate, java.sql.Date](
    utilDate => new java.sql.Date(utilDate.toDateTimeAtStartOfDay(DateTimeZone.UTC).getMillis),
    sqlDate => new LocalDate(sqlDate.getTime, DateTimeZone.UTC)
  )

  class RestrictionEntryTable(tag: Tag) extends Table[RestrictionEntry](tag, "rs_restrictions") {
    def created = (createdById, createdByName, createdAt) <> (AuditEntry.tupled, AuditEntry.unapply)
    def modified = (lastModifiedById, lastModifiedByName, lastModifiedAt) <> (AuditEntry.tupled, AuditEntry.unapply)

    def * = (id, name, startDate, endDate, noEndFlag, message, indicator, created, modified) <> ((RestrictionEntry.apply _).tupled, RestrictionEntry.unapply)

    def id = column[Int]("id")

    def name = column[String]("name")

    def startDate = column[LocalDate]("start_date")

    def endDate = column[Option[LocalDate]]("end_date")

    def noEndFlag = column[Int]("no_end_flag")

    def message = column[String]("message")

    def indicator = column[Char]("indicator")

    def createdAt = column[DateTime]("created_at")

    def createdById = column[String]("created_by_id")

    def createdByName = column[String]("created_by_name")

    def lastModifiedAt = column[DateTime]("last_modified_at")

    def lastModifiedById = column[String]("last_modified_by_id")

    def lastModifiedByName = column[String]("last_modified_by_name")
  }

  class RestrictionDataTable(tag: Tag) extends Table[RestrictionData](tag, "rs_restrictions_data") {

    def * = (id, lastModifiedAt, clubNumber, item, subCategory, vendorNumber) <> ((RestrictionData.apply _).tupled, RestrictionData.unapply)

    def id = column[Int]("id")

    def lastModifiedAt = column[DateTime]("last_modified_at")

    def clubNumber = column[Option[Long]]("club_number")

    def item = column[Option[Int]]("item_number")

    def subCategory = column[Option[Int]]("sub_category")

    def vendorNumber = column[Option[Int]]("vendor_number")
  }

  val restrictionDataTableQuery = TableQuery[RestrictionDataTable]
  val restrictionEntryTableQuery = TableQuery[RestrictionEntryTable]

  def fetchForId(id: Int) : DBIO[Seq[RestrictionEntry]] = {
    restrictionEntryTableQuery.filter(_.id === id).result
  }

  def fetchRestrictionEntry : DBIO[Seq[RestrictionEntry]] = restrictionEntryTableQuery.filter(_.indicator === ACTIVE).result

  def fetchRestrictionData(list: Seq[(Int, DateTime)]) : DBIO[Seq[RestrictionData]] = {
    restrictionDataTableQuery.filter(row => list.map(e => row.id === e._1 && row.lastModifiedAt === e._2).reduceLeft(_ || _)).result
  }

  def fetchRestrictionDataLogForId(id: Int) : DBIO[Map[(Int, DateTime), Seq[RestrictionData]]] =
    restrictionDataTableQuery.filter(_.id === id).result.map {
      _.groupBy(row => (row.id, row.lastModifiedAt)).map {
        case (k, v) => Map(k -> v)
      }.reduceLeft(_ ++ _)
    }
}

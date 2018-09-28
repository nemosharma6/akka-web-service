package com.nimish.akka.persistence.dao

import com.nimish.akka.Constants._
import com.nimish.akka.FIELDS._
import com.nimish.akka.persistence.models._
import com.nimish.akka.utils.{ActorsConfig, Config, Logger}
import org.joda.time._
import slick.jdbc.JdbcProfile
import slick.lifted.CanBeQueryCondition

import scala.concurrent.Future

class RoadshowDao(val profile: JdbcProfile) extends Config with ActorsConfig with Logger {

  import profile.api._

  implicit val utilDate2SqlDate = MappedColumnType.base[DateTime, java.sql.Timestamp](
    utilDate => new java.sql.Timestamp(utilDate.getMillis),
    sqlDate => new DateTime(sqlDate.getTime)
  )

  implicit val utilLocalDate2SqlDate = MappedColumnType.base[LocalDate, java.sql.Date](
    utilDate => new java.sql.Date(utilDate.toDateTimeAtStartOfDay(DateTimeZone.UTC).getMillis),
    sqlDate => new LocalDate(sqlDate.getTime, DateTimeZone.UTC)
  )

  case class MaybeFilter[X, Y](query: slick.lifted.Query[X, Y, Seq]) {
    def filter[T, R: CanBeQueryCondition](data: Option[T])(f: T => X => R) = {
      data.map(v => MaybeFilter(query.withFilter(f(v)))).getOrElse(this)
    }
  }

  class RoadshowTable(tag: Tag) extends Table[Roadshow](tag, "roadshows") {

    def roadshowNumber = column[Int]("roadshow_number", O.PrimaryKey)

    def roadshowDescription = column[String]("roadshow_description")

    def vendorNumber = column[Int]("vendor_number")

    def eventCodeId = column[Int]("event_code_id")

    def eventDesc = column[String]("event_desc")

    def startDate = column[LocalDate]("start_date")

    def endDate = column[LocalDate]("end_date")

    def returnAddressId = column[Option[Int]]("ret_add_id")

    def manned = column[Int]("manned")

    def createdAt = column[DateTime]("created_at")

    def lastModifiedAt = column[DateTime]("last_modified_at")

    def * = (roadshowNumber, roadshowDescription.rtrim, vendorNumber, eventCodeId, eventDesc.rtrim, startDate, endDate, returnAddressId, manned, createdAt, lastModifiedAt) <> ((Roadshow.apply _).tupled, Roadshow.unapply)

    def buyerView = (roadshowNumber, roadshowDescription.rtrim, vendorNumber, eventCodeId, eventDesc.rtrim, manned) <> ((RoadshowBuyer.apply _).tupled, RoadshowBuyer.unapply)

    def returnAddressFk = foreignKey("fk_ret_add", returnAddressId, ReturnAddressTQ)(_.id)
  }

  class RoadshowClubTable(tag: Tag) extends Table[RoadshowClub](tag, "rs_clubs") {

    def * = (roadshowNumber, clubNumber, clubName.rtrim, market, clubInstructions, status, startDate, endDate, palletsRequired) <> ((RoadshowClub.apply _).tupled, RoadshowClub.unapply)

    def buyerView = (roadshowNumber, clubNumber, clubName.rtrim, market, status, startDate, endDate, palletsRequired) <> ((RoadshowClubBuyer.apply _).tupled, RoadshowClubBuyer.unapply)

    def roadshowNumber = column[Int]("roadshow_number")

    def clubNumber = column[Long]("club_number")

    def clubName = column[String]("club_name")

    def market = column[Int]("market")

    def status = column[String]("status")

    def clubInstructions = column[String]("club_instructions")

    def startDate = column[LocalDate]("start_date")(utilLocalDate2SqlDate)

    def endDate = column[LocalDate]("end_date")(utilLocalDate2SqlDate)

    def palletsRequired = column[Int]("pallets_required")

    def clubPk = primaryKey("clubPK", (roadshowNumber, clubNumber))

    def rdFk = foreignKey("rdFK", roadshowNumber, RoadshowTQ)(_.roadshowNumber)
  }

  class RoadshowItemTable(tag: Tag) extends Table[RoadshowItem](tag, "rs_items") {

    def * = (roadshowNumber, itemNumber, itemName.rtrim) <> ((RoadshowItem.apply _).tupled, RoadshowItem.unapply)

    def roadshowNumber = column[Int]("roadshow_number")

    def itemNumber = column[Int]("item_number")

    def subCategory = column[Int]("sub_category")

    def itemName = column[Option[String]]("item_desc")

    def itemPk = primaryKey("itemPK", (roadshowNumber, itemNumber))

    def rdFk = foreignKey("rdFK", roadshowNumber, RoadshowTQ)(_.roadshowNumber)
  }

  class BlockedClubTable(tag: Tag) extends Table[BlockedClub](tag, "blocked_clubs") {

    def storeNbr = column[Long]("club_number", O.PrimaryKey)

    def * = storeNbr <> (BlockedClub.apply, BlockedClub.unapply)
  }

  class PalletEntryTable(tag: Tag) extends Table[PalletCountEntry](tag, "pallet_count") {
    def * = (clubNumber, date, count) <> ((PalletCountEntry.apply _).tupled, PalletCountEntry.unapply)

    def clubNumber = column[Long]("club_number")

    def date = column[LocalDate]("date")(utilLocalDate2SqlDate)

    def count = column[Int]("count")
  }

  class ReturnAddressTable(tag: Tag) extends Table[ReturnAddress](tag, "return_address") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)

    def address = column[String]("address")

    def vendorNumber = column[Int]("vendor_number")

    def * = (id, address) <> ((ReturnAddress.apply _).tupled, ReturnAddress.unapply)
  }

  val RoadshowClubTQ = TableQuery[RoadshowClubTable]
  val RoadshowTQ = TableQuery[RoadshowTable]
  val RoadshowItemTQ = TableQuery[RoadshowItemTable]
  val BlockedClubTQ = TableQuery[BlockedClubTable]
  val PalletTQ = TableQuery[PalletEntryTable]
  val ReturnAddressTQ = TableQuery[ReturnAddressTable]

  private def allQuery(request: GenericFilterRequest) = {

    val rds = MaybeFilter(RoadshowTQ)
      .filter(request.vendorNumber)(v => row => row.vendorNumber === v)
      .filter(request.roadshowNumber)(r => row => row.roadshowNumber inSet r)
      .query

    val cls = MaybeFilter(RoadshowClubTQ)
      .filter(request.startDate)(s => row => row.endDate >= s)
      .filter(request.endDate)(e => row => row.startDate <= e)
      .filter(request.clubNumber)(cl => row => row.clubNumber inSet cl)
      .filter(request.status)(st => row => row.status === st)
      .query

    val clFinal = (cls joinLeft BlockedClubTQ on (_.clubNumber === _.storeNbr)).filter(_._2.isEmpty)

    for {
      ((r, a), i) <- rds joinLeft ReturnAddressTQ on (_.returnAddressId === _.id) joinLeft RoadshowItemTQ on (_._1.roadshowNumber === _.roadshowNumber)
      c <- clFinal if c._1.roadshowNumber === r.roadshowNumber
    } yield ((r.buyerView, a), c._1, i)
  }

  private def allQueryNoItem(request: GenericFilterRequest) = {

    val rds = MaybeFilter(RoadshowTQ)
      .filter(request.roadshowNumber)(r => row => row.roadshowNumber inSet r)
      .query

    val cls = MaybeFilter(RoadshowClubTQ)
      .filter(request.startDate)(s => row => row.endDate >= s)
      .filter(request.endDate)(e => row => row.startDate <= e)
      .filter(request.clubNumber)(cl => row => row.clubNumber inSet cl)
      .filter(request.status)(st => row => row.status === st)
      .query

    val clFinal = (cls joinLeft BlockedClubTQ on (_.clubNumber === _.storeNbr)).filter(_._2.isEmpty)

    for {
      rd <- rds
      cl <- clFinal if cl._1.roadshowNumber === rd.roadshowNumber
    } yield (rd, cl._1)
  }

  def getAddress(request: VendorAddressRequest): DBIO[Seq[ReturnAddress]] = {
    ReturnAddressTQ.filter(_.vendorNumber === request.vendorNumber).result
  }

  private def allQueryWithSubCat(request: GenericFilterRequest) = {

    val rds = MaybeFilter(RoadshowTQ)
      .filter(request.roadshowNumber)(r => row => row.roadshowNumber inSet r)
      .query

    val cls = MaybeFilter(RoadshowClubTQ)
      .filter(request.startDate)(s => row => row.endDate >= s)
      .filter(request.endDate)(e => row => row.startDate <= e)
      .filter(request.clubNumber)(cl => row => row.clubNumber inSet cl)
      .filter(request.status)(st => row => row.status === st)
      .query

    val clFinal = (cls joinLeft BlockedClubTQ on (_.clubNumber === _.storeNbr)).filter(_._2.isEmpty)


    for {
      (r, i) <- rds join RoadshowItemTQ on (_.roadshowNumber === _.roadshowNumber)
      c <- clFinal if c._1.roadshowNumber === r.roadshowNumber
    } yield (r.buyerView, c._1.buyerView, i.subCategory)
  }

  def dateDiff(d1: Rep[LocalDate], d2: Rep[LocalDate]): Rep[Int] =
    SimpleFunction.binary[LocalDate, LocalDate, Int]("datediff").apply(d1, d2)

  def fetchPalletsForLongerEvents(request: GenericFilterRequest): DBIO[Seq[(Long, LocalDate, LocalDate, Option[Int])]] = {
    (
      for {
        cl <- MaybeFilter(RoadshowClubTQ)
          .filter(request.clubNumber)(cl => row => row.clubNumber inSet cl)
          .filter(request.endDate)(en => row => row.startDate <= en)
          .filter(request.startDate)(st => row => row.endDate >= Seq(LocalDate.now(DateTimeZone.UTC), st).max)
          .query if cl.status.inSet(Seq(APPROVED, PROPOSED)) && dateDiff(cl.endDate, cl.startDate) > THIRTY_DAY

        pl <- PalletTQ if cl.clubNumber === pl.clubNumber && pl.date >= cl.startDate && pl.date <= cl.endDate
      } yield (cl, pl)

      ).groupBy(row => (row._1.clubNumber, row._1.startDate, row._1.endDate))
      .map {
        case (k, v) => (k._1, k._2, k._3, v.map(_._2.count).max)
      }.result
  }

  def findRoadshowForEdit(roadshowNumber: Int): DBIO[Seq[(RoadshowBuyer, Seq[RoadshowClub], Seq[Option[RoadshowItem]], Option[ReturnAddress])]] = {
    val filter = GenericFilterRequest(None, Some(Seq(roadshowNumber)))
    val compiledQuery = Compiled(allQuery(filter))
    compiledQuery.result.map {
      _.groupBy(_._1).map {
        case (k, v) => (k._1, v.map(_._2).distinct, v.map(_._3).distinct, k._2)
      }.toSeq
    }
  }

  def findRoadshowByFilter(request: GenericFilterRequest): DBIO[Seq[((RoadshowBuyer, RoadshowClubBuyer, Option[ReturnAddress]), Seq[Option[RoadshowItem]])]] = {
    val compiledQuery = Compiled(allQuery(request).map(e => (e._1, e._2.buyerView, e._3)))
    compiledQuery.result.map {
      _.groupBy(each => (each._1, each._2)).map {
        case (k, v) => ((k._1._1, k._2, k._1._2), v.map(_._3).distinct)
      }.toSeq
    }
  }

  def findRoadshowForClubView(request: GenericFilterRequest): DBIO[Seq[((RoadshowBuyer, RoadshowClub, Option[ReturnAddress]), Seq[Option[RoadshowItem]])]] = {
    val compiledQuery = Compiled(allQuery(request))
    compiledQuery.result.map {
      _.groupBy(each => (each._1, each._2)).map {
        case (k, v) => ((k._1._1, k._2, k._1._2), v.map(_._3).distinct)
      }.toSeq
    }
  }

  def findRoadshowBuyerView(request: GenericFilterRequest): DBIO[Seq[(RoadshowBuyer, RoadshowClubBuyer)]] = {
    val compiledQuery = Compiled(allQueryNoItem(request).map(e => (e._1.buyerView, e._2.buyerView)))
    compiledQuery.result.map(e => e.map(each => (each._1, each._2)))
  }

  def fetchRoadshowMetaInfo(request: GenericFilterRequest): DBIO[Seq[(RoadshowBuyer, RoadshowClubBuyer)]] = {
    val compiledQuery = Compiled(allQueryNoItem(request).map(e => (e._1.buyerView, e._2.buyerView)))
    compiledQuery.result.map(e => e.map(each => (each._1, each._2)))
  }

  def findRoadshowBuyerViewTemp(request: GenericFilterRequest): DBIO[Seq[(RoadshowBuyer, RoadshowClubBuyer, Int)]] = {
    val compiledQuery = Compiled(allQueryWithSubCat(request))
    compiledQuery.result
  }

  def findClubsSchedule(clubsPaletteRequest: ClubsPalletRequest): DBIO[Seq[PalletResponse]] = {
    PalletTQ.filter(row => row.clubNumber.inSet(clubsPaletteRequest.clubs) && row.date >= clubsPaletteRequest.startDate
      && row.date <= clubsPaletteRequest.endDate)
      .result
      .map {
        _.groupBy(_.clubNumber).map {
          case (k, v) => PalletResponse(k, v.map(e => PalletCount(e.date, e.palletUsed)))
        }.toSeq
      }
  }

  implicit def LocalDateOrdering: Ordering[LocalDate] = Ordering.fromLessThan(_ isBefore _)

  def calculateClubsPalletsReport(clubsPalletRequest: ClubsPalletRequest, pallet: Seq[PalletResponse], clubs: Map[Int, ClubInfo]): Seq[PalletReport] = {
    pallet.map { p =>
      val cl = clubs(p.clubNbr.toInt)
      PalletReport(p.clubNbr, cl.city, cl.stateCode, cl.regionNbr, cl.districtNbr, cl.palletCapacity, p.dates)
    }
  }

  def fetchPalletCount(request: GenericFilterRequest): DBIO[Seq[PalletCountEntry]] = {
    MaybeFilter(PalletTQ)
      .filter(request.clubNumber)(cl => row => row.clubNumber inSet cl)
      .filter(request.startDate)(st => row => row.date >= st.minusDays(THIRTY_DAY))
      .filter(request.endDate)(en => row => row.date <= en.plusDays(THIRTY_DAY))
      .query
      .result
  }

  def calculateConflicts(rds: Seq[(RoadshowBuyer, RoadshowClubBuyer, Seq[Int])], cf: Seq[Conflicts]): Seq[Future[(RoadshowBuyer, RoadshowClubWithConflict)]] = {
    val proposedEvents = rds.filter(_._2.status == PROPOSED)
    val today = LocalDate.now(DateTimeZone.UTC)

    cf.isEmpty match {
      case true =>  rds.map(r => Future(r._1, RoadshowClubWithConflict.create(r._2, None)))
      case false =>
        proposedEvents.map { pr =>
          Future {
            val conflicts = cf.filter(c => c.startDate.isBefore(today) && c.endDate.get.isAfter(today)).map { c =>
              val conflictExists = c.conflictType match {
                case DUPLICATION =>
                  c.field match {
                    case Some(EVENT_CODE_ID) => rds.find(r => r._1.eventCode == pr._1.eventCode && roadshowProximity(r._2, pr._2) <= c.timeFrame)
                    case Some(VENDOR_NUMBER) => rds.find(r => r._1.vendorNumber == pr._1.vendorNumber && roadshowProximity(r._2, pr._2) <= c.timeFrame)
                    case _ => None
                  }
                //same supplier and club
                case TIME_PERIOD =>
                  c.subCategory match {
                    case Some(sb) =>
                      val targetSubCat = pr._3.intersect(sb)
                      rds.find(r => r._1.vendorNumber == pr._1.vendorNumber && r._2.clubNumber == pr._2.clubNumber && r._3.intersect(targetSubCat).nonEmpty
                        && pr._3.intersect(sb).nonEmpty && roadshowProximity(r._2, pr._2) <= c.timeFrame)
                    case _ => None
                  }
              }

              conflictExists match {
                case Some(_) => Some(c.name)
                case None => None
              }
            }

            val conflictNames = conflicts.flatten.nonEmpty match {
              case true => Some(conflicts.flatten)
              case false => None
            }

            (pr._1, RoadshowClubWithConflict.create(pr._2, conflictNames))
          }
        } ++ rds.filter(_._2.status != PROPOSED).map(r => Future(r._1, RoadshowClubWithConflict.create(r._2, None)))
    }
  }

  def roadshowProximity(rd1: RoadshowClubBuyer, rd2: RoadshowClubBuyer) = {
    Days.daysBetween(rd1.endDate, rd2.startDate).getDays
  }

  def roadshowProximity(date: LocalDate, rd: RoadshowClubBuyer) = {
    Days.daysBetween(date, rd.startDate).getDays
  }
}

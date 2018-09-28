package com.nimish.akka.services

import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import com.nimish.akka.Constants._
import com.nimish.akka.{Caching, InterServiceCall}
import com.nimish.akka.auth.config.models.{UserInfo, WireUserInfo}
import com.nimish.akka.auth.config.{AuthConfig, AuthorizationEngine}
import com.nimish.akka.auth.routes.SecuredRoute
import com.nimish.akka.mappings._
import com.nimish.akka.persistence.models._
import com.nimish.akka.utils._
import org.joda.time.{DateTime, DateTimeZone, Days, LocalDate}
import com.nimish.akka.ErrorHandling._
import com.nimish.akka.auth.ResponseCodes._

import scala.collection.immutable.HashMap
import scala.concurrent.Future

class RoadshowInfoService(persistence: PersistenceModule with DbModule) extends ActorsConfig with AuthConfig with JsonMappings with SecuredRoute
  with AuthorizationEngine with Decrypt with InterServiceCall {

  import persistence._

  val routes =
    pathEndOrSingleSlash {
      get {
        complete("EMS - Fetch Roadshow Info service is up and running")
      }
    } ~
      handleExceptions(errorHandler) {
        clubUserRoute ~ unsecuredRoutes
      }

  def clubUserRoute =
    pathPrefix("capi") {
      pathPrefix("roadshow") {
        post {
          pathPrefix("meta") {
            entity(as[RoadshowFilterRequest]) { request =>
              authenticateWithClubSessionCookie {
                getRoadshowMetaInformationForClubUI(request)
              }
            }
          } ~
            pathPrefix("clubDownload") {
              entity(as[RoadshowFilterRequest]) { request =>
                authenticateWithClubSessionCookie {
                  getRoadshowClubDownloadForClubUI(request)
                }
              }
            } ~
            pathPrefix("pallet") {
              entity(as[ClubsPalletRequest]) { request =>
                authenticateWithClubSessionCookie {
                  getClubsPalletsForClubUI(request)
                }
              }
            }
        }
      }
    }

  def getRoadshowClubDownloadForClubUI(request: RoadshowFilterRequest)(user: WireUserInfo) =
    pathEndOrSingleSlash {
      authorize(user.endTime.isAfter(DateTime.now(DateTimeZone.UTC))) {
        val genericRequest = GenericFilterRequest.create(request, Some(APPROVED))
        onSuccess(db.run(roadshows.findRoadshowForClubView(genericRequest))) { rds =>
          rds.isEmpty match {
            case true => complete(OK, RDS_014.message)
            case false =>
              onSuccess(fetchVendorMapping(rds.map(_._1._1.vendorNumber).distinct)) { vdr =>
                encodeResponseWith(Gzip)(clubDownload(rds, vdr))
              }
          }
        }
      }
    }

  def getRoadshowMetaInformationForClubUI(request: RoadshowFilterRequest)(user: WireUserInfo) =
    pathEndOrSingleSlash {
      authorize(user.endTime.isAfter(DateTime.now(DateTimeZone.UTC))) {
        val genericRequest = GenericFilterRequest.create(request, Some(APPROVED))
        onSuccess(db.run(roadshows.fetchRoadshowMetaInfo(genericRequest))) { rds =>
          rds.isEmpty match {
            case true => complete(OK, RDS_014.message)
            case false =>
              onSuccess(fetchVendorMapping(rds.map(_._1.vendorNumber).distinct)) { vdr =>
                encodeResponseWith(Gzip)(complete(rds.map(RoadshowMetaResponse.create(_, vdr))))
              }
          }
        }
      }
    }

  def getClubsPalletsForClubUI(clubsPalletRequest: ClubsPalletRequest)(user: WireUserInfo) =
    authorize(user.endTime.isAfter(DateTime.now(DateTimeZone.UTC))) {
      pathEndOrSingleSlash {
        onSuccess(fetchPalletAndClub(clubsPalletRequest)) { (schedule, capacityMap) =>
          encodeResponseWith(Gzip)(complete(OK, schedule.map(PalletResponseWithCapacity.create(_, capacityMap))))
        }
      }
    }

  def unsecuredRoutes =
    pathPrefix("api") {
      pathPrefix("roadshow") {
        get {
          pathPrefix("edit") {
            authenticateWithSessionID {
              getDetailsForRoadshowEdit
            }
          } ~
            pathPrefix("restriction") {
              authenticateWithSessionID {
                getRestrictions
              }
            } ~
            pathPrefix("restriction" / IntNumber) { id =>
              authenticateWithSessionID {
                getRestrictionLog(id)
              }
            } ~
            pathPrefix("conflict") {
              authenticateWithSessionID {
                getConflicts
              }
            } ~
            pathPrefix("conflict" / IntNumber) { id =>
              authenticateWithSessionID {
                getConflictLog(id)
              }
            }
        } ~
          post {
            pathPrefix("pallet") {
              entity(as[ClubsPalletRequest]) { request =>
                authenticateWithSessionID {
                  getClubsPallets(request)
                }
              }
            } ~
              pathPrefix("palletReport") {
                entity(as[ClubsPalletRequest]) { request =>
                  authenticateWithSessionID {
                    getPalletReport(request)
                  }
                }
              } ~
              pathPrefix("filter") {
                entity(as[RoadshowFilterRequest]) { request =>
                  authenticateWithSessionID {
                    getRoadshowsByFilter(request)
                  }
                }
              } ~
              pathPrefix("filterWithConflict") {
                entity(as[RoadshowFilterRequest]) { request =>
                  authenticateWithSessionID {
                    getRoadshowsByFilterWithConflict(request)
                  }
                }
              } ~
              pathPrefix("address") {
                entity(as[VendorAddressRequest]) { request =>
                  authenticateWithSessionID {
                    fetchVendorAddress(request)
                  }
                }
              }
          }
      }
    }

  def getRoadshowsByFilterWithConflict(request: RoadshowFilterRequest)(user: UserInfo) =
    authorize(user.isBuyer) {
      entity(as[RoadshowFilterRequest]) { request =>
        pathEndOrSingleSlash {
          onSuccess(fetchRoadshowAndVendorTemp(request)) { (mp, v, rds, lpt, cf) =>
            rds.isEmpty match {
              case true => complete(OK, RDS_014.message)
              case false =>
                onSuccess(Future.sequence(roadshows.calculateConflicts(rds, cf))) { rdWithConflict =>
                  encodeResponseWith(Gzip)(complete(buyerViewForSmallEventsTemp(request.startDate, mp, v, rdWithConflict) ++ buyerViewForLargeEventsTemp(lpt, v, rdWithConflict)))
                }
            }
          }
        }
      }
    }

  def getConflicts(user: UserInfo) =
    authorize(user.isBuyer) {
      pathEndOrSingleSlash {
        onSuccess(db.run(roadshowConflict.fetchConflictEntry)) { cf =>
          cf.isEmpty match {
            case true => complete(OK, RDS_022.message)
            case false =>
              onSuccess(fetchConflictData(cf)) {
                case (None, _) => complete(InternalServerError, RDS_012.message)
                case (Some(mp), data) => encodeResponseWith(Gzip)(complete(cf.map(c => Conflicts.create(c, data(c.id), mp))))
              }
          }
        }
      }
    }

  private def fetchConflictData(name: Seq[ConflictEntry]): Future[(Option[Map[String, String]], Map[Int, Seq[ConflictData]])] = {
    val encryptedData = name.flatMap(e => Seq(e.created.byUserId, e.created.byUserName, e.modified.byUserName, e.modified.byUserId))
    val decryptedData = decryptData(encryptedData)
    val conflictData = roadshowConflict.fetchConflictData(name.map(e => (e.id, e.modified.at)))

    for {
      mp <- decryptedData
      data <- conflictData
    } yield (mp, data.groupBy(_.id).map(d => Map(d._1 -> d._2)).reduceLeft(_ ++ _))
  }

  def getConflictLog(id: Int)(user: UserInfo) =
    authorize(user.isBuyer) {
      onSuccess(db.run(roadshowConflict.fetchForId(id))) { cf =>
        cf.isEmpty match {
          case true => complete(OK, RDS_022.message)
          case false =>
            onSuccess(fetchConflictLog(cf)) {
              case (Some(mp), data) => encodeResponseWith(Gzip)(complete(cf.map(r => Conflicts.create(r, data((r.id, r.modified.at)), mp))))
              case (None, _) => complete(InternalServerError, RDS_012.message)
            }
        }
      }
    }

  private def fetchConflictLog(name: Seq[ConflictEntry]): Future[(Option[Map[String, String]], Map[(Int, DateTime), Seq[ConflictData]])] = {
    val encryptedData = name.flatMap(n => Seq(n.created.byUserId, n.created.byUserName, n.modified.byUserName, n.modified.byUserId))
    val decryptedData = decryptData(encryptedData)
    val conflictData = roadshowConflict.fetchConflictDataLogForId(name.head.id)

    for {
      mp <- decryptedData
      data <- conflictData
    } yield (mp, data)
  }

  def getRestrictions(user: UserInfo) =
    authorize(user.isBuyer) {
      pathEndOrSingleSlash {
        onSuccess(db.run(roadshowRestriction.fetchRestrictionEntry)) { rst =>
          rst.isEmpty match {
            case true => complete(OK, RDS_022.message)
            case false =>
              onSuccess(fetchRestrictionData(rst)) {
                case (None, _) => complete(InternalServerError, RDS_012.message)
                case (Some(mp), data) => encodeResponseWith(Gzip)(complete(rst.map(rs => Restriction.create(rs, data(rs.id), mp))))
              }
          }
        }
      }
    }

  private def fetchRestrictionData(name: Seq[RestrictionEntry]): Future[(Option[Map[String, String]], Map[Int, Seq[RestrictionData]])] = {
    val encryptedData = name.flatMap(e => Seq(e.created.byUserId, e.created.byUserName, e.modified.byUserName, e.modified.byUserId))
    val decryptedData = decryptData(encryptedData)
    val restrictionData = roadshowRestriction.fetchRestrictionData(name.map(e => (e.id, e.modified.at)))

    for {
      mp <- decryptedData
      data <- restrictionData
    } yield (mp, data.groupBy(_.id).map(d => Map(d._1 -> d._2)).reduceLeft(_ ++ _))
  }

  def getRestrictionLog(id: Int)(user: UserInfo) =
    authorize(user.isBuyer) {
      onSuccess(db.run(roadshowRestriction.fetchForId(id))) { rst =>
        rst.isEmpty match {
          case true => complete(OK, RDS_022.message)
          case false =>
            onSuccess(fetchRestrictionLog(rst)) {
              case (Some(mp), data) => encodeResponseWith(Gzip)(complete(rst.map(r => Restriction.create(r, data((r.id, r.modified.at)), mp))))
              case (None, _) => complete(InternalServerError, RDS_012.message)
            }
        }
      }
    }

  private def fetchRestrictionLog(name: Seq[RestrictionEntry]): Future[(Option[Map[String, String]], Map[(Int, DateTime), Seq[RestrictionData]])] = {
    val encryptedData = name.flatMap(n => Seq(n.created.byUserId, n.created.byUserName, n.modified.byUserName, n.modified.byUserId))
    val decryptedData = decryptData(encryptedData)
    val restrictionData = roadshowRestriction.fetchRestrictionDataLogForId(name.head.id)

    for {
      mp <- decryptedData
      data <- restrictionData
    } yield (mp, data)
  }

  def getDetailsForRoadshowEdit(user: UserInfo) = {
    pathPrefix(IntNumber) { roadshowNumber =>
      authorize(user.isBuyer || user.isVendor)
      onSuccess(fetchRoadshowAndClub(roadshowNumber)) { (roadshow, mp) =>
        roadshow.isEmpty match {
          case true => complete(NotFound, RDS_014.message)
          case _ =>
            onSuccess(fetchVendorMapping(roadshow.map(_._1.vendorNumber).distinct)) { vdr =>
              val cl = roadshow.head._2.map(e => RoadshowClubForEdit.create(e, mp))
              encodeResponseWith(Gzip)(complete(Seq(RoadshowEdit.create(roadshow.head, vdr, cl))))
            }
        }
      }
    }
  }

  def getClubsPallets(clubsPalletRequest: ClubsPalletRequest)(user: UserInfo) =
    authorize(user.isVendor || user.isBuyer) {
      pathEndOrSingleSlash {
        onSuccess(fetchPalletAndClub(clubsPalletRequest)) { (schedule, capacityMap) =>
          encodeResponseWith(Gzip)(complete(OK, schedule.map(PalletResponseWithCapacity.create(_, capacityMap))))
        }
      }
    }

  def getPalletReport(clubsPalletRequest: ClubsPalletRequest)(user: UserInfo) =
    authorize(user.isVendor || user.isBuyer) {
      pathEndOrSingleSlash {
        onSuccess(fetchPalletAndClub(clubsPalletRequest)) { (schedule, clubMap) =>
          encodeResponseWith(Gzip) {
            complete(OK, persistence.roadshows.calculateClubsPalletsReport(clubsPalletRequest, schedule, clubMap))
          }
        }
      }
    }

  def getRoadshowsByFilter(roadshowFilterRequest: RoadshowFilterRequest)(user: UserInfo) =
    authorize(user.isBuyer) {
      pathEndOrSingleSlash {
        onSuccess(fetchRoadshowAndVendor(roadshowFilterRequest)) { (mp, v, rds, lpt) =>
          rds.isEmpty match {
            case true => complete(OK, RDS_014.message)
            case false =>
              val st = roadshowFilterRequest.startDate
              encodeResponseWith(Gzip)(complete(buyerViewForSmallEvents(st, mp, v, rds) ++ buyerViewForLargeEvents(lpt, v, rds)))
          }
        }
      }
    } ~
      authorize(user.isVendor) {
        pathEndOrSingleSlash {
          roadshowFilterRequest.vendorNumber match {
            case Some(v) => authorize(user.hasVendor(v)) {
              getRoadshowsForSupplier(roadshowFilterRequest)
            }
            case None =>
              user.Vendors.isEmpty match {
                case true => complete(OK, RDS_015.message)
                case false =>
                  val request = RoadshowFilterRequest(Some(user.Vendors.head.Number), None, None, None, roadshowFilterRequest.startDate, roadshowFilterRequest.endDate)
                  getRoadshowsForSupplier(request)
              }
          }
        }
      }

  private def getRoadshowsForSupplier(request: RoadshowFilterRequest) =
    onSuccess(db.run(roadshows.findRoadshowByFilter(GenericFilterRequest.create(request)))) { rds =>
      rds.isEmpty match {
        case true => complete(OK, RDS_014.message)
        case _ =>
          onSuccess(fetchVendorMapping(rds.map(_._1._1.vendorNumber).distinct)) { vdr =>
            encodeResponseWith(Gzip)(complete(rds.map(RoadshowWithItem.create(_, vdr))))
          }
      }
    }

  def fetchVendorAddress(request: VendorAddressRequest)(user: UserInfo) =
    authorize(user.isBuyer) {
      pathEndOrSingleSlash {
        onSuccess(db.run(roadshows.getAddress(request))) { add =>
          add.isEmpty match {
            case true => complete(OK, RDS_028.message)
            case false => encodeResponseWith(Gzip)(complete(OK, add))
          }
        }
      }
    } ~
      authorize(user.isVendor && user.hasVendor(request.vendorNumber)) {
        pathEndOrSingleSlash {
          onSuccess(db.run(roadshows.getAddress(request))) { add =>
            add.isEmpty match {
              case true => complete(OK, RDS_028.message)
              case false => encodeResponseWith(Gzip)(complete(OK, add))
            }
          }
        }
      }

  private def fetchRoadshowAndVendor(request: RoadshowFilterRequest): Future[(Map[Long, Array[Int]], Map[Int, String], Seq[(RoadshowBuyer, RoadshowClubBuyer)], Seq[PalletCountList])] = {
    val genericRequest = GenericFilterRequest.create(request)
    val rds = db.run(roadshows.findRoadshowBuyerView(genericRequest))
    val plt = db.run(roadshows.fetchPalletCount(genericRequest)).map { p =>
      p.groupBy(_.clubNumber).map {
        case (k, v) =>
          val sorted = v.sortBy(_.date).map(_.palletUsed).toArray
          HashMap(k -> sorted)
      }.toSeq
    }

    val longerEvents = db.run(roadshows.fetchPalletsForLongerEvents(genericRequest))

    for {
      r <- rds
      v <- fetchVendorMapping(r.map(_._1.vendorNumber).distinct)
      lpt <- longerEvents
      p <- plt
    } yield (p.reduce(_ ++ _), v, r, lpt.map(PalletCountList.tupled.apply(_)))
  }

  private def buyerViewForSmallEvents(st: LocalDate, mp: Map[Long, Array[Int]], vdr: Map[Int, String], rds: Seq[(RoadshowBuyer, RoadshowClubBuyer)]) = {
    rds.filter(r => Seq(APPROVED, PROPOSED).contains(r._2.status) && (r._2.endDate.isAfter(LocalDate.now(DateTimeZone.UTC)) || r._2.endDate.isEqual(LocalDate.now(DateTimeZone.UTC))) && Days.daysBetween(r._2.startDate, r._2.endDate).getDays <= THIRTY_DAY).map { r =>
      mp.keySet.contains(r._2.clubNumber) match {
        case true =>
          val left = Days.daysBetween(st.minusDays(THIRTY_DAY), r._2.startDate).getDays
          val right = Days.daysBetween(st.minusDays(THIRTY_DAY), r._2.endDate).getDays
          val ar = mp(r._2.clubNumber)

          val mx = right >= ar.length match {
            case true => ar.length - 1
            case false => right
          }

          val palletList = (left to mx).map(e => ar(e))
          palletList.isEmpty match {
            case false => RoadshowBuyerView.create((r._1, r._2, Some(palletList.max)), vdr)
            case true => RoadshowBuyerView.create((r._1, r._2, None), vdr)
          }

        case false => RoadshowBuyerView.create((r._1, r._2, None), vdr)
      }
    } ++
      rds.filter(r => Seq(REJECTED, CANCELLED).contains(r._2.status) || r._2.endDate.isBefore(LocalDate.now(DateTimeZone.UTC))).map { r =>
        RoadshowBuyerView.create((r._1, r._2, None), vdr)
      }
  }

  private def buyerViewForLargeEvents(palletList: Seq[PalletCountList], vdr: Map[Int, String], rds: Seq[(RoadshowBuyer, RoadshowClubBuyer)]) = {
    rds.filter(r => Seq(APPROVED, PROPOSED).contains(r._2.status) && (r._2.endDate.isAfter(LocalDate.now(DateTimeZone.UTC)) || r._2.endDate.isEqual(LocalDate.now(DateTimeZone.UTC))) && Days.daysBetween(r._2.startDate, r._2.endDate).getDays > THIRTY_DAY).map { r =>
      val pU = palletList.filter(e => e.clubNumber == r._2.clubNumber && e.startDate == r._2.startDate && e.endDate == r._2.endDate)
        .map(_.palletUsed).headOption match {
        case Some(p) => p
        case None => None
      }
      RoadshowBuyerView.create((r._1, r._2, pU), vdr)
    }
  }

  private def fetchRoadshowAndClub(roadshowNumber: Int): Future[(Seq[(RoadshowBuyer, Seq[RoadshowClub], Seq[Option[RoadshowItem]], Option[ReturnAddress])], Map[Int, ClubInfo])] = {
    val rds = db.run(roadshows.findRoadshowForEdit(roadshowNumber))
    val cls = Caching.clubCacheOp(PALLET_KEY)

    for {
      r <- rds
      c <- cls
    } yield (r, c)
  }

  def fetchRoadshowAndVendorTemp(request: RoadshowFilterRequest): Future[(Map[Long, Array[Int]], Map[Int, String], Seq[(RoadshowBuyer, RoadshowClubBuyer, Seq[Int])], Seq[PalletCountList], Seq[Conflicts])] = {
    val genericRequest = GenericFilterRequest.create(request)
    val rds = db.run(roadshows.findRoadshowBuyerViewTemp(genericRequest))
    val plt = db.run(roadshows.fetchPalletCount(genericRequest)).map { p =>
      p.groupBy(_.clubNumber).map {
        case (k, v) =>
          val sorted = v.sortBy(_.date).map(_.palletUsed).toArray
          HashMap(k -> sorted)
      }.toSeq
    }

    val longerEvents = db.run(roadshows.fetchPalletsForLongerEvents(genericRequest))
    val conflicts = db.run(roadshowConflict.fetch)

    for {
      r <- rds
      v <- fetchVendorMapping(r.map(_._1.vendorNumber).distinct)
      lpt <- longerEvents
      p <- plt
      cf <- conflicts
    } yield (p.reduce(_ ++ _), v, r.groupBy(e => (e._1, e._2)).map(e => (e._1._1, e._1._2, e._2.map(_._3).distinct)).toSeq, lpt.map(PalletCountList.tupled.apply(_)), cf.map(c => Conflicts.tempCreate(c)))
  }

  def buyerViewForSmallEventsTemp(st: LocalDate, mp: Map[Long, Array[Int]], vdr: Map[Int, String], rds: Seq[(RoadshowBuyer, RoadshowClubWithConflict)]) = {
    rds.filter(r => Seq(APPROVED, PROPOSED).contains(r._2.status) && (r._2.endDate.isAfter(LocalDate.now(DateTimeZone.UTC)) || r._2.endDate.isEqual(LocalDate.now(DateTimeZone.UTC))) && Days.daysBetween(r._2.startDate, r._2.endDate).getDays <= THIRTY_DAY).map { r =>
      mp.keySet.contains(r._2.clubNumber) match {
        case true =>
          val left = Days.daysBetween(st.minusDays(THIRTY_DAY), r._2.startDate).getDays
          val right = Days.daysBetween(st.minusDays(THIRTY_DAY), r._2.endDate).getDays
          val ar = mp(r._2.clubNumber)

          val mx = right >= ar.length match {
            case true => ar.length - 1
            case false => right
          }

          val palletList = (left to mx).map(e => ar(e))
          RoadshowBuyerViewTemp.create((r._1, r._2, Some(palletList.max)), vdr)

        case false => RoadshowBuyerViewTemp.create((r._1, r._2, None), vdr)
      }
    } ++
      rds.filter(r => Seq(REJECTED, CANCELLED).contains(r._2.status) || r._2.endDate.isBefore(LocalDate.now(DateTimeZone.UTC))).map { r =>
        RoadshowBuyerViewTemp.create((r._1, r._2, None), vdr)
      }
  }

  def buyerViewForLargeEventsTemp(palletList: Seq[PalletCountList], vdr: Map[Int, String], rds: Seq[(RoadshowBuyer, RoadshowClubWithConflict)]) = {
    rds.filter(r => Seq(APPROVED, PROPOSED).contains(r._2.status) && (r._2.endDate.isAfter(LocalDate.now(DateTimeZone.UTC)) || r._2.endDate.isEqual(LocalDate.now(DateTimeZone.UTC))) && Days.daysBetween(r._2.startDate, r._2.endDate).getDays > THIRTY_DAY).map { r =>
      val pU = palletList.filter(e => e.clubNumber == r._2.clubNumber && e.startDate == r._2.startDate && e.endDate == r._2.endDate)
        .map(_.palletUsed).headOption match {
        case Some(p) => p
        case None => None
      }
      RoadshowBuyerViewTemp.create((r._1, r._2, pU), vdr)
    }
  }

  private def fetchPalletAndClub(request: ClubsPalletRequest): Future[(Seq[PalletResponse], Map[Int, ClubInfo])] = {
    val rds = db.run(roadshows.findClubsSchedule(request))
    val cls = Caching.clubCacheOp(PALLET_KEY)

    for {
      r <- rds
      c <- cls
    } yield (r, c)
  }

  private def fetchVendorMapping(vendorList: Seq[Int]): Future[Map[Int, String]] = {
    for {
      vdr <- Caching.vendorCacheOp(VENDOR_KEY)
      newVendors <- {
        val vdrList = vdr.keySet.toSeq
        val diff = vendorList.diff(vdrList)
        log.info("List of missing vendors : " + diff)
        diff.isEmpty match {
          case false => fetchMissingVendorsWithRetry(diff) map (l => Map(l map { a => a.number -> a.name }: _*))
          case true => Future(Seq())
        }
      }
      completeVdr <- newVendors.isEmpty match {
        case true => Future(vdr)
        case false =>
          log.info("Populating cache with missing vendors...")
          Caching.vendorCacheOp(VENDOR_KEY, Some(vdr ++ newVendors))
      }
    } yield completeVdr
  }

  private def clubDownload(rds: Seq[((RoadshowBuyer, RoadshowClub, Option[ReturnAddress]), Seq[Option[RoadshowItem]])], vdr: Map[Int, String]) = {
    val tmp = rds.map(r => ClubDownloadRoadshow.create(r, vdr))
    val t = tmp.groupBy(e => (e.clubNumber.get, e.clubName.get, e.market.get)).map {
      case (k, v) => (k, v)
    }.toSeq
    complete(OK, t.map(ClubViewDownload.create))
  }
}

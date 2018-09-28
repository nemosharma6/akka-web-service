package com.nimish.akka.mappings

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.nimish.akka.persistence.models.{PalletCountList, RoadshowBuyerViewTemp, RoadshowClubWithFlagBuyer, _}
import org.joda.time.{DateTime, LocalDate}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import spray.json._

trait JsonMappings extends SprayJsonSupport with DefaultJsonProtocol {

  implicit object LocalDateFormat extends JsonFormat[LocalDate] {
    def write(date: LocalDate) = JsString(date.toString("MM/dd/yyyy"))

    def read(json: JsValue) = json match {
      case JsString(rawDate) => LocalDate.parse(rawDate, dateFormatterLocalDate.get())
      case error => deserializationError(s"Expected JsString, got $error")
    }
  }

  implicit def LocalDateOrdering: Ordering[LocalDate] = Ordering.fromLessThan(_ isBefore _)

  implicit object DateFormat extends JsonFormat[DateTime] {
    def write(date: DateTime) = JsString(dateToString(date))

    def read(json: JsValue) = json match {
      case JsString(rawDate) => parseDateString(rawDate)
      case error => deserializationError(s"Expected JsString, got $error")
    }
  }

  implicit val dateFormatterLocalDate = new ThreadLocal[DateTimeFormatter] {
    override def initialValue() = DateTimeFormat.forPattern("MM/dd/yyyy")
  }

  implicit val dateFormatter = new ThreadLocal[DateTimeFormatter] {
    override def initialValue() = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
  }

  implicit def DateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)

  implicit def dateToString(date: DateTime): String = dateFormatter.get().print(date)

  implicit def parseDateString(date: String): DateTime = dateFormatter.get().parseDateTime(date)

  implicit def returnAddressFormat = jsonFormat2(ReturnAddress.apply)

  implicit val roadshowFormat = jsonFormat11(Roadshow.apply)
  implicit val roadshowClubFormat = jsonFormat9(RoadshowClub.apply)
  implicit val roadshowClubForEditFormat = jsonFormat10(RoadshowClubForEdit.apply)

  implicit val roadshowItemFormat = jsonFormat3(RoadshowItem)
  implicit val roadshowClubWithConflict = jsonFormat9(RoadshowClubWithConflict.apply)
  implicit val roadshowResponseFormat = jsonFormat8(RoadshowMetaResponse.apply)
  implicit val roadshowRequestFormat = jsonFormat6(RoadshowFilterRequest.apply)
  implicit val roadshowClubWithFlagBuyerTempFormat = jsonFormat10(RoadshowClubWithFlagBuyerTemp.apply)

  implicit val clubsPalRequestFormat = jsonFormat3(ClubsPalletRequest)
  implicit val paletteCountFormat = jsonFormat2(PalletCount)
  implicit val clubsScheduleResponseFormat = jsonFormat2(PalletResponse)
  implicit val palletResponseWithCapacityFormat = jsonFormat3(PalletResponseWithCapacity.apply)

  implicit val clubDownloadRoadshowFormat = jsonFormat17(ClubDownloadRoadshow.apply)
  implicit val clubViewDownloadFormat = jsonFormat4(ClubViewDownload.apply)

  implicit val auditEntryFormat = jsonFormat3(AuditEntry)
  implicit val roadshowRestrictionFormat = jsonFormat13(Restriction.apply)
  implicit val roadshowConflictFormat = jsonFormat11(Conflicts.apply)
  implicit val roadshowBuyerFormat = jsonFormat6(RoadshowBuyer.apply)

  implicit val roadshowClubBuyerFormat = jsonFormat8(RoadshowClubBuyer.apply)
  implicit val roadshowClubWithFlagBuyerFormat = jsonFormat9(RoadshowClubWithFlagBuyer.apply)
  implicit val roadshowWithFlagBuyerViewFormat = jsonFormat8(RoadshowBuyerView.apply)
  implicit val roadshowEditTempFormat = jsonFormat10(RoadshowEdit.apply)
  implicit val roadshowWithItemTempFormat = jsonFormat9(RoadshowWithItem.apply)

  implicit val clubInfoFormat = jsonFormat7(ClubInfo)
  implicit val vendorInfoFormat = jsonFormat3(VendorInfo)
  implicit val vendorAddressRequestFormat = jsonFormat1(VendorAddressRequest)

  implicit val palletResponseReportFormat = jsonFormat7(PalletReport)

  implicit val vendorListFormat = jsonFormat1(VendorList)
  implicit val palletCountListFormat = jsonFormat4(PalletCountList)

  implicit val roadshowBuyerViewTempFormat = jsonFormat7(RoadshowBuyerViewTemp.apply)
}

package com.nimish.akka

import java.io.{BufferedReader, InputStreamReader}

import akka.http.scaladsl.model.RequestEntity
import com.nimish.rest.api.HTTPMethod
import com.nimish.akka.mappings.JsonMappings
import com.nimish.akka.persistence.models.{ClubInfo, VendorInfo, VendorList}
import com.nimish.akka.utils.{ActorsConfig, Config, Logger}
import org.apache.http.entity.ContentType
import org.apache.http.{Header, HttpResponse}
import spray.json._

import scala.concurrent.Future
import scala.concurrent.duration._

trait InterServiceCall extends Config with JsonMappings with ActorsConfig with Logger {

  private def retry[T](f: => Future[T], delay: FiniteDuration, c: Int): Future[T] =
    f.recoverWith {
      case ex: Exception if c > 0 => {
        log.info(s"Error : ${ex.getMessage}. Retry for Club Api. Count is : $c")
        akka.pattern.after(delay, system.scheduler)(retry(f, delay, c - 1))
      }
    }

  def fetchMissingVendors(vendorNumbers: Seq[Int]): Future[Seq[VendorInfo]] = {
    val postRequest = VendorList(vendorNumbers)
    val client = HTTPMethod.POST.getRestClient
    for {
      response <- Future(client.submit(userService, new Array[Header](0), ContentType.APPLICATION_JSON, postRequest.toJson.toString.getBytes))
      entity <- Future(getResponseBody(response))
    } yield {
      client.releaseConnection()
      entity.parseJson.convertTo[Seq[VendorInfo]]
    }
  }

  def fetchMissingVendorsWithRetry(vendorNumbers: Seq[Int]): Future[Seq[VendorInfo]] = {
    retry(fetchMissingVendors(vendorNumbers), waitInterval.seconds, retryCount)
  }

  def fetchVendors: Future[Seq[VendorInfo]] = {
    val client = HTTPMethod.GET.getRestClient
    for {
      response <- Future(client.submit(userService))
      entity <- Future(getResponseBody(response))
    } yield {
      client.releaseConnection()
      entity.parseJson.convertTo[Seq[VendorInfo]]
    }
  }

  def fetchClubsWithRetry: Future[Seq[ClubInfo]] = {
    retry(fetchClubs, waitInterval.seconds, retryCount)
  }

  private def fetchClubs: Future[Seq[ClubInfo]] = {
    val client = HTTPMethod.GET.getRestClient
    for {
      response <- Future(client.submit(clubService))
      entity <- Future(getResponseBody(response))
    } yield {
      client.releaseConnection()
      entity.parseJson.convertTo[Seq[ClubInfo]]
    }
  }

  def getResponseBody(response: HttpResponse): String = {
    val buffer = new StringBuilder
    if (response != null) {
      val reader = new BufferedReader(new InputStreamReader(response.getEntity.getContent))
      reader.readLine().map(buffer.append)
    }
    buffer.toString
  }
}

package com.nimish.akka.utils

import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpRequest, RequestEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.nimish.akka.mappings.JsonMappings
import org.joda.time.{DateTime, DateTimeZone}
import com.nimish.rest.api.HTTPMethod
import org.apache.http.entity.ContentType
import org.apache.http.{Header, HttpResponse}
import java.io.BufferedReader
import java.io.InputStreamReader
import spray.json._

import scala.concurrent.Future

trait Decrypt extends ActorsConfig with JsonMappings with Logger{

  def decryptData(list: Seq[String]): Future[Option[Map[String, String]]] = {
    val start = DateTime.now(DateTimeZone.UTC).getMillis
    for {
      values <- newDecrypt(list).map(each => scala.util.parsing.json.JSON.parseFull(each) match {
        case Some(map: Map[String, String]@unchecked) => Some(map)
        case Some(_) => None
        case None => None
      })
    } yield {
      log.info("Decryption Runtime : " + (DateTime.now(DateTimeZone.UTC).getMillis - start))
      values
    }
  }

  def decrypt(list: Seq[String]) = {
    for {
      request <- Marshal(list).to[RequestEntity]
      response <- Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = decryptUrl, entity = request))
      entity <- Unmarshal(response.entity.withContentType(ContentTypes.`application/json`)).to[String]
    } yield entity
  }

  def newDecrypt(list: Seq[String]) = {
    val client = HTTPMethod.POST.getRestClient
    for {
      response <- Future(client.submit(decryptUrl, new Array[Header](0), ContentType.APPLICATION_JSON, list.toJson.toString.getBytes))
      entity <- Future(getResponse(response))
    } yield {
      client.releaseConnection
      entity
    }
  }

  def getResponse(response: HttpResponse): String = {
    val buffer = new StringBuilder
    if (response != null) {
      val reader = new BufferedReader(new InputStreamReader(response.getEntity.getContent))
      reader.readLine.map(buffer.append(_))
      reader.close
    }
    buffer.toString
  }
}

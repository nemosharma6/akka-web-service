package com.nimish.akka

import java.sql.SQLException

import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.ExceptionHandler
import com.nimish.akka.auth.ResponseCodes.{RDS_009, RDS_010, RDS_013}
import com.nimish.akka.auth.mappings.JsonMappings
import com.nimish.akka.utils._
import spray.json.JsonParser.ParsingException

object ErrorHandling extends Logger with JsonMappings with ActorsConfig {

  val errorHandler = ExceptionHandler {

    case ex: ParsingException =>
      log.error(ex.printStackTrace().toString)
      complete(InternalServerError, RDS_009.message)

    case ex: SQLException =>
      log.error(ex.printStackTrace().toString)
      complete(InternalServerError, RDS_010.message)

    case ex: Exception =>
      log.error(ex.printStackTrace().toString)
      complete(InternalServerError, RDS_013.message)

  }
}

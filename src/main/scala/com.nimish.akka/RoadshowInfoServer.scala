package com.nimish.akka

import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.directives.DebuggingDirectives
import com.nimish.akka.services._
import com.nimish.akka.utils._
import com.nimish.akka.auth.SupportedMethodsOptionsProvider._

object RoadshowInfoServer extends App with Directives with Config with ActorsConfig with Logger {
  log.info("Starting server ...")

  val roadshowRoutes = new RoadshowInfoService(PersistenceModule).routes

  Http().bindAndHandle(
    DebuggingDirectives.logRequestResult("RoadshowInfoServer", Logging.InfoLevel)(roadshowRoutes)
    , httpInterface
    , httpPort)

  log.info(s"Server to fetch roadshow info started at http://$httpInterface:$httpPort/")
}

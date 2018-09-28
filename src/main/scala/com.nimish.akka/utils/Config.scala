package com.nimish.akka.utils

import com.typesafe.config.ConfigFactory

trait Config {
  val config = ConfigFactory.load(
    s"application-conf/${System.getProperty("app.run.env","dev")}.conf"
  )

  private val httpConfig = config.getConfig("http")
  val httpInterface = httpConfig.getString("interface")
  val httpPort = httpConfig.getInt("port")
  val decryptUrl = config.getString("decrypt-url")

  val itemDBConfig = config.getConfig("item-db")
  val itemDBName = itemDBConfig.getString("name")
  val itemTable = itemDBConfig.getString("table")

  val vendorDBConfig = config.getConfig("vendor-db")
  val vendorDBName = vendorDBConfig.getString("name")
  val vendorTable = vendorDBConfig.getString("table")

  val waitInterval = config.getInt("services.wait-interval")
  val retryCount = config.getInt("services.retry-count")
  val clubService = config.getString("services.club-service.host")
  val userService = config.getString("services.user-service.host")
}

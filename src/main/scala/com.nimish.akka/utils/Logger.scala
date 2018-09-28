package com.nimish.akka.utils

import akka.event.{Logging, LoggingAdapter}

trait Logger extends ActorsConfig {
  val log: LoggingAdapter = Logging(system, this.getClass)
}

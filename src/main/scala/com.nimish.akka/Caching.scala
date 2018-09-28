package com.nimish.akka

import com.nimish.akka.persistence.models.ClubInfo
import com.nimish.akka.utils.{ActorsConfig, Config, Logger}
import spray.caching._

import scala.concurrent.duration._
import scala.concurrent.Future

object Caching extends ActorsConfig with Config with InterServiceCall with Logger{

  val vendorCache: Cache[Map[Int, String]] = LruCache.apply(timeToLive = 24.hour)
  val clubCache: Cache[Map[Int, ClubInfo]] = LruCache.apply(timeToLive = 24.hour)

  def vendorCacheOp[T](key: T, vendorList : Option[Map[Int, String]] = None) : Future[Map[Int, String]] = {
    vendorList match {
      case Some(vdr) =>
        vendorCache.clear
        vendorCache(key){
          log.info("Cache updated with Vendor Details")
          vdr
        }
      case None => vendorCache(key){
        log.info("Fetching Vendor details from DB2 and Caching...")
        fetchVendors map(l => Map(l map { a => a.number -> a.name }: _*))
      }
    }
  }

  def clubCacheOp[T](key: T) : Future[Map[Int, ClubInfo]] =
    clubCache(key){
      log.info("Fetching Pallet details from Club Service and Caching...")
      fetchClubsWithRetry map(l => Map(l map { a => a.clubNumber -> a }: _*))
    }

}

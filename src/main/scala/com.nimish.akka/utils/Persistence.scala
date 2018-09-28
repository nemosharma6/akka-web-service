package com.nimish.akka.utils

import com.nimish.akka.persistence.dao._
import slick.basic.DatabaseConfig
import slick.dbio.DBIO
import slick.jdbc.{DB2Profile, JdbcProfile}

import scala.concurrent.Future


trait Profile {
  val profile: JdbcProfile
}


trait DbModule extends Profile {
  val db: JdbcProfile#Backend#Database

  implicit def executeOperation[T](databaseOperation: DBIO[T]): Future[T] = {
    db.run(databaseOperation)
  }
}

trait PersistenceModule {
  val roadshows: RoadshowDao
  val roadshowRestriction: RoadshowRestrictionDao
  val roadshowConflict: RoadshowConflictDao
}

object PersistenceModule extends PersistenceModule with DbModule with Config{
  private val dbConfig: DatabaseConfig[JdbcProfile] = DatabaseConfig.forConfig("mariadb", config)

  override implicit val profile: JdbcProfile = dbConfig.profile
  override implicit val db: JdbcProfile#Backend#Database = dbConfig.db

  override val roadshows = new RoadshowDao(profile)
  override val roadshowRestriction: RoadshowRestrictionDao = new RoadshowRestrictionDao(profile)
  override val roadshowConflict: RoadshowConflictDao = new RoadshowConflictDao(profile)
}

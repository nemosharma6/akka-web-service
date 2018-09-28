import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.nimish.akka.auth.config.AuthConfig
import com.nimish.akka.auth.config.models.{UserGroup, UserInfo, Vendor}
import com.nimish.akka.mappings.JsonMappings
import com.nimish.akka.persistence.dao.{RoadshowConflictDao, RoadshowDao, RoadshowRestrictionDao}
import com.nimish.akka.services.RoadshowInfoService
import com.nimish.akka.utils._
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.scalatest.{Matchers, WordSpec}
import org.specs2.mock.Mockito
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import org.joda.time.LocalDate

class RoadshowInfoSpec extends WordSpec with Matchers with ScalatestRouteTest with Mockito with JsonMappings with Config with AuthConfig {

  private val persistence = new PersistenceModule with DbModule {
    private val dbConfig: DatabaseConfig[JdbcProfile] = DatabaseConfig.forConfig("mariadb", config)

    override implicit val profile: JdbcProfile = dbConfig.profile
    override implicit val db: JdbcProfile#Backend#Database = dbConfig.db

    override val roadshows: RoadshowDao = mock[RoadshowDao]
    override val roadshowRestriction: RoadshowRestrictionDao = mock[RoadshowRestrictionDao]
    override val roadshowConflict: RoadshowConflictDao = mock[RoadshowConflictDao]
  }

  val service = new RoadshowInfoService(persistence)
  val route = service.routes

  override val dateFormatter = new ThreadLocal[DateTimeFormatter] {
    override def initialValue() = DateTimeFormat.forPattern("MM/dd/yyyy")
  }

  val date = LocalDate.parse("01/01/2017", dateFormatter.get())
  val vendorUserGroup = UserGroup("",132)
  val buyerUserGroup = UserGroup("",127)
  val vendor = Vendor("",123456,"","")
  val vendorUserInfo = UserInfo("a0a00g0","","","","","","","","","","",0,"","","","","",Seq(vendorUserGroup),Seq(""),Seq(vendor))
  val buyerUserInfo = UserInfo("a0a00g0","","","","","","","","","","",0,"","","","","",Seq(buyerUserGroup),Seq(""),Seq())

  "The service to submit Roadshow" should {

    "return a message that services are up and running for GET requests to the root path" in {
      Get() ~> route ~> check {
        responseAs[String] shouldEqual "EMS - Fetch Roadshow Info service is up and running"
      }
    }
  }
}

package com.github.opengrabeso.mixtio

import java.io.InputStream
import java.security.MessageDigest
import java.util
import java.util.Properties
import java.time.ZonedDateTime
import com.google.api.client.http.{GenericUrl, HttpRequest}
import com.google.api.client.http.json.JsonHttpContent
import com.fasterxml.jackson.databind.JsonNode
import com.google.api.client.json.gson.GsonFactory

import scala.jdk.CollectionConverters._
import common.Util._
import common.model._
import shared.Timing

import scala.collection.immutable.SortedMap

object Main extends common.Formatting {

  import RequestUtils._

  private val md = MessageDigest.getInstance("SHA-256")

  def digest(bytes: Array[Byte]): String = {
    val digestBytes = (0:Byte) +: md.digest(bytes) // prepend 0 byte to avoid negative sign
    BigInt(digestBytes).toString(16)
  }

  def digest(str: String): String = digest(str.getBytes)

  case class SecretResult(appId: String, appSecret: String, mapboxToken: String, darkSkySecret: String, error: String)

  def secret: SecretResult = {
    val filename = "/secret.txt"
    try {
      val secretStream = Main.getClass.getResourceAsStream(filename)
      val lines = scala.io.Source.fromInputStream(secretStream).getLines()
      SecretResult(lines.next(), lines.next(), lines.next(), lines.next(), "")
    } catch {
      case _: NullPointerException => // no file found
        SecretResult("", "", "", "", s"Missing $filename, app developer should check README.md")
      case _: Exception =>
        SecretResult("", "", "", "", s"Bad $filename, app developer should check README.md")
    }
  }

  def devMode: Boolean = {
    val prop = new Properties()
    prop.load(getClass.getResourceAsStream("/config.properties"))
    prop.getProperty("devMode").toBoolean
  }

  case class StravaAuthResult(token: String, refreshToken: String, refreshExpire: Long, mapboxToken: String, id: String, name: String, sessionId: String) {
    // userId used for serialization, needs to be stable, cannot be created from a token
    lazy val userId: String = id
  }

  private def buildAuthJson: util.HashMap[String, String] = {
    val json = new util.HashMap[String, String]()
    val SecretResult(clientId, clientSecret, mapboxToken, _, _) = secret

    json.put("client_id", clientId)
    json.put("client_secret", clientSecret)
    json
  }

  private def authRequest(json: util.HashMap[String, String]): JsonNode = {
    val content = new JsonHttpContent(new GsonFactory(), json)

    val request = requestFactory.buildPostRequest(new GenericUrl("https://www.strava.com/oauth/token"), content)
    val response = try {
      request.execute() // TODO: async?
    } catch {
      case ex: Exception =>
        println(s"authRequest failed: $ex")
        throw ex
    }

    jsonMapper.readTree(response.getContent)
  }

  def stravaAuth(code: String): StravaAuthResult = {

    val SecretResult(clientId, clientSecret, mapboxToken, _, _) = secret

    val json = buildAuthJson
    json.put("code", code)
    json.put("grant_type", "authorization_code")

    val responseJson = authRequest(json)

    val token = responseJson.path("access_token").textValue
    val refreshToken = responseJson.path("refresh_token").textValue
    val refreshExpire = responseJson.path("expires_at").longValue

    val athleteJson = responseJson.path("athlete")
    val id = athleteJson.path("id").numberValue.toString
    val name = athleteJson.path("firstname").textValue + " " + athleteJson.path("lastname").textValue

    val sessionId = "full-session-" + System.currentTimeMillis().toString
    val auth = StravaAuthResult(token, refreshToken, refreshExpire, mapboxToken, id, name, sessionId)
    rest.RestAPIServer.createUser(auth)
    auth
  }

  def stravaAuthRefresh(previous: StravaAuthResult): StravaAuthResult = {
    // if not expired yet (with some space left), use it
    // if expired or about to expire soon, request a new one
    // https://developers.strava.com/docs/authentication/:
    //  If the application’s access tokens for the user are expired or will expire in one hour (3,600 seconds) or less, a new access token will be returned
    val now = System.currentTimeMillis / 1000
    val validUntil = previous.refreshExpire - 3600
    if (now > validUntil) {
      val json = buildAuthJson
      json.put("refresh_token", previous.refreshToken)
      json.put("grant_type", "refresh_token")

      val responseJson = authRequest(json)

      val token = responseJson.path("access_token").textValue
      val refreshToken = responseJson.path("refresh_token").textValue
      val refreshExpire = responseJson.path("expires_at").longValue

      val auth = previous.copy(token = token, refreshToken = refreshToken, refreshExpire = refreshExpire)
      rest.RestAPIServer.createUser(auth)
      auth
    } else {
      rest.RestAPIServer.createUser(previous)
      previous
    }
  }


  def loadActivityId(json: JsonNode): ActivityHeader = {
    // https://developers.strava.com/docs/reference/#api-Activities-getActivityById
    val name = json.path("name").textValue
    val id = json.path("id").longValue
    val time = ZonedDateTime.parse(json.path("start_date").textValue)
    val sportName = json.path("type").textValue
    val duration = json.path("elapsed_time").intValue
    val distance = json.path("distance").doubleValue
    val hasGPS = !json.path("start_latlng").isMissingNode && !json.path("start_latlng").isNull
    val hasHR = json.path("has_heartrate").booleanValue
    val avgSpeed = json.path("average_speed").doubleValue
    val maxSpeed = json.path("max_speed").doubleValue
    val actDigest = digest(json.toString)

    def sportFromName(name: String): Event.Sport = {
      try {
        Event.Sport.withName(sportName)
      } catch {
        case _: NoSuchElementException => SportId.Workout
      }
    }

    val actId = ActivityId(FileId.StravaId(id), actDigest, name, time, time.plusSeconds(duration), sportFromName(sportName), distance)
    ActivityHeader(actId,hasGPS,hasHR,SpeedStats(avgSpeed, avgSpeed, maxSpeed))
  }

  def parseStravaActivities(content: InputStream): Seq[ActivityHeader] = {
    val responseJson = jsonMapper.readTree(content)

    val stravaActivities = (0 until responseJson.size).map { i =>
      val actI = responseJson.get(i)
      loadActivityId(actI)
    }
    stravaActivities
  }

  def lastStravaActivities(auth: StravaAuthResult, count: Int): Seq[ActivityId] = {
    val timing = Timing.start()
    val uri = "https://www.strava.com/api/v3/athlete/activities"
    val request = buildGetRequest(uri, auth.token, s"per_page=$count")

    val ret = parseStravaActivities(request.execute().getContent).map(_.id)
    timing.logTime(s"lastStravaActivities ($count)")
    ret
  }

  private val normalCount = 15

  def recentStravaActivities(auth: StravaAuthResult): Seq[ActivityId] = {
    lastStravaActivities(auth, normalCount)
  }

  def recentStravaActivitiesHistory(auth: StravaAuthResult, countMultiplier: Double = 1): (Seq[ActivityId], Seq[ActivityId]) = {
    val allActivities = lastStravaActivities(auth, (normalCount * countMultiplier).toInt)
    allActivities.splitAt(normalCount)
  }


  def stravaActivitiesNotStaged(auth: StravaAuthResult): Seq[ActivityId] = {
    val stravaActivities = recentStravaActivities(auth)
    if (stravaActivities.nonEmpty) {
      val notBefore = stravaActivities.map(_.startTime).min
      val storedActivities = stagedActivities(auth, notBefore)
      // do not display the activities which are already staged
      stravaActivities diff storedActivities
    } else {
      stravaActivities
    }
  }

  object namespace {
    // stage are data visible to the user
    val stage = "stage"
    // editable data - not listed in staging
    val edit = "edit"
    // session storage
    def session(session: String) = "session/" + session
    // file upload progress
    val uploadProgress = "upload-progress"
    // upload - invisible data, used to hand data to the background upload tasks
    def upload(session: String) = "upload-" + session
    // upload results - report upload status and resulting id
    def uploadResult(session: String) = "upload-result-" + session
    // user settings
    val settings = "settings"
  }

  def stagedActivities(auth: StravaAuthResult, notBefore: ZonedDateTime): Seq[ActivityHeader] = {
    val storedActivities = {
      def isNotBeforeByName(name: String) = {
        val md = Storage.metadataFromFilename(name)
        md.get("startTime").forall(timeString => ZonedDateTime.parse(timeString) >= notBefore)
      }
      val d = Storage.enumerate(namespace.stage, auth.userId, Some(isNotBeforeByName))
      d.flatMap { a =>
        Storage.load[ActivityHeader](a._1)
      }
    }
    storedActivities.toVector
  }

  @SerialVersionUID(10L)
  case object NoActivity

  def getEventsFrom(authToken: String, id: String): ActivityEvents = {

    println(s"Download from strava $id")
    val uri = s"https://www.strava.com/api/v3/activities/$id"
    val request = buildGetRequest(uri, authToken, "")

    val responseJson = jsonMapper.readTree(request.execute().getContent)

    val actId = loadActivityId(responseJson).id
    val startDateStr = responseJson.path("start_date").textValue
    val startTime = ZonedDateTime.parse(startDateStr)

    object StravaActivityStreams extends ActivityEvents.ActivityStreams {
      // https://strava.github.io/api/v3/streams/
      //private val allStreams = Seq("time", "latlng", "distance", "altitude", "velocity_smooth", "heartrate", "cadence", "watts", "temp", "moving", "grade_smooth")
      private val wantStreams = Seq("time", "latlng", "distance", "altitude", "heartrate", "cadence", "watts"/*, "temp"*/)

      private val streamTypes = wantStreams.mkString(",")

      private val uri = s"https://www.strava.com/api/v3/activities/$id/streams/$streamTypes"
      private val request = buildGetRequest(uri, authToken, "")

      private val response = request.execute().getContent

      private val responseJson = jsonMapper.readTree(response)

      val streams = responseJson.elements.asScala.iterator.to(Iterable)

      def getData[T](stream: LazyList[JsonNode], get: JsonNode => T): Vector[T] = {
        if (stream.isEmpty) Vector()
        else stream.head.path("data").asScala.map(get).toVector
      }

      def getDataByName[T](name: String, get: JsonNode => T): Vector[T] = {
        val stream = streams.filter(_.path("type").textValue == name).to(LazyList)
        getData(stream, get)
      }

      def getAttribByName(name: String): (String, Seq[Int]) = {
        (name, getDataByName(name, _.asInt))
      }

      private def loadGpsPair(gpsItem: JsonNode) = {
        val elements = gpsItem.elements
        val lat = elements.next.asDouble
        val lng = elements.next.asDouble
        GPSPoint(lat, lng, None)(None)
      }

      val timeRelValues = getDataByName("time", _.asInt)
      val distValues = getDataByName("distance", _.asDouble)
      val latlngValues = getDataByName("latlng", loadGpsPair)
      val altValues = getDataByName("altitude", _.asDouble)

      val latLngAltValues = if (altValues.isEmpty) latlngValues else {
        (latlngValues zip altValues).map { case (gps,alt) =>
            gps.copy(elevation = Some(alt.toInt))(Some(gps.accuracy))
        }
      }

      val timeValues = timeRelValues.map ( t => startTime.plusSeconds(t))

      val attributeValues: Seq[(String, Seq[Int])] = Seq(
        getAttribByName("cadence"),
        getAttribByName("watts"),
        getAttribByName("temp"),
        getAttribByName("heartrate")
      )

      val dist = new DataStreamDist(SortedMap(timeValues zip distValues:_*))
      val latlng = new DataStreamGPS(SortedMap(timeValues zip latLngAltValues:_*))
      val attributes =  attributeValues.filter(_._2.nonEmpty).flatMap { case (name, values) =>
          name match {
            case "heartrate" => Some(new DataStreamHR(SortedMap(timeValues zip values:_*)))
            case _ => Some(new DataStreamAttrib(name, SortedMap(timeValues zip values:_*)))
          }
      }

    }

    val laps = {


      val requestLaps = buildGetRequest(s"https://www.strava.com/api/v3/activities/$id/laps", authToken, "")

      val response = requestLaps.execute().getContent

      val lapsJson = jsonMapper.readTree(response)

      val lapTimes = (for (lap <- lapsJson.elements.asScala) yield {
        val lapTimeStr = lap.path("start_date").textValue
        ZonedDateTime.parse(lapTimeStr)
      }).toList


      lapTimes.filter(_ > actId.startTime)
    }

    val segments: Seq[Event] = {
      val segmentList = responseJson.path("segment_efforts").asScala.toList
      segmentList.flatMap { seg =>
        val segStartTime = ZonedDateTime.parse(seg.path("start_date").textValue)
        val segName = seg.path("name").textValue
        val segDuration = seg.path("elapsed_time").intValue
        val segPrivate = seg.path("segment").path("private").booleanValue
        val segmentId = seg.path("segment").path("id").longValue
        Seq(
          StartSegEvent(segName, segPrivate, segmentId, segStartTime),
          EndSegEvent(segName, segPrivate, segmentId, segStartTime.plusSeconds(segDuration))
        )
      }
    }

    // recompute start / end times based on the real data (Strava sometimes stores duration too short)
    val startTimes = Seq(actId.startTime) ++ StravaActivityStreams.latlng.stream.headOption.map(_._1) ++ StravaActivityStreams.attributes.flatMap(_.stream.headOption.map(_._1))
    val endTimes = Seq(actId.endTime) ++ StravaActivityStreams.latlng.stream.lastOption.map(_._1) ++ StravaActivityStreams.attributes.flatMap(_.stream.lastOption.map(_._1))

    val realStartTime = startTimes.min
    val realEndTime = endTimes.max

    ActivityEvents.processActivityStream(actId.copy(startTime = realStartTime, endTime = realEndTime), StravaActivityStreams, laps, segments)

  }

  def adjustEvents(events: ActivityEvents, eventsInput: Seq[String]): ActivityEvents = {
    val ee = events.events zip eventsInput

    val lapsAndSplits: Array[Event] = ee.flatMap { case (e, ei) =>
      e match {
        case ev@EndEvent(_) => Some(ev)
        case _ =>
          if (ei.startsWith("split")) {
            val sportName = ei.substring("split".length)
            Some(SplitEvent(e.stamp, Event.Sport.withName(sportName)))
          } else ei match {
            case "lap" => Some(LapEvent(e.stamp))
            case "end" => Some(EndEvent(e.stamp))
            case _ => None
          }
      }
    }
    events.copy(events = lapsAndSplits)
  }

  def jsDateRange(startTime: ZonedDateTime, endTime: ZonedDateTime): String = {
    s"""formatDateTime("$startTime") + "..." + formatTime("$endTime") """
  }

  def jsResult(func: String) = {

    val toRun = s"function () {return $func}()"

    <script>document.write({xml.Unparsed(toRun)})</script>
  }

}






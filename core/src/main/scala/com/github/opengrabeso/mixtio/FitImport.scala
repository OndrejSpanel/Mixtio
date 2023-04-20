package com.github.opengrabeso.mixtio

import java.io.InputStream

import com.garmin.fit._
import java.time.{LocalDateTime, ZoneId, ZoneOffset, ZonedDateTime}

import common.Util._
import common.model.{Event, _}
import common.model.FileId._

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Ondra on 20.10.2016.
  */
object FitImport {

  def fromTimestamp(time: Long): ZonedDateTime = {
    val localTime = LocalDateTime.ofEpochSecond(time + DateTime.OFFSET / 1000, 0, ZoneOffset.UTC)
    ZonedDateTime.of(localTime, ZoneOffset.UTC)
  }

  def decodeLatLng(lat: Int, lng: Int, elev: Option[java.lang.Float]): GPSPoint = {
    val longLatScale = (1L << 31).toDouble / 180
    GPSPoint(lat / longLatScale, lng / longLatScale, elev.map(_.toInt))(None)
  }


  def apply(filename: String, digest: String, in: InputStream): Option[ActivityEvents] = {
    val decode = new Decode
    try {

      val gpsBuffer = ArrayBuffer[(ZonedDateTime, GPSPoint)]() // Time -> Lat / Long
      val hrBuffer = ArrayBuffer[(ZonedDateTime, Int)]()
      val cadenceBuffer = ArrayBuffer[(ZonedDateTime, Int)]()
      val powerBuffer = ArrayBuffer[(ZonedDateTime, Int)]()
      val distanceBuffer = ArrayBuffer[(ZonedDateTime, Double)]()
      val lapBuffer = ArrayBuffer[ZonedDateTime]()

      case class FitHeader(sport: Option[Event.Sport] = None)

      var header = FitHeader()

      val listener = new MesgListener {

        override def onMesg(mesg: Mesg): Unit = {
          mesg.getNum match {
            case MesgNum.RECORD =>
              val timestamp = Option(mesg.getField(RecordMesg.TimestampFieldNum)).map(_.getLongValue)
              val heartrate = Option(mesg.getField(RecordMesg.HeartRateFieldNum)).map(_.getIntegerValue)
              val distance = Option(mesg.getField(RecordMesg.DistanceFieldNum)).map(_.getFloatValue)
              val posLat = Option(mesg.getField(RecordMesg.PositionLatFieldNum)).map(_.getIntegerValue)
              val posLong = Option(mesg.getField(RecordMesg.PositionLongFieldNum)).map(_.getIntegerValue)
              val elev = Option(mesg.getField(RecordMesg.AltitudeFieldNum)).map(_.getFloatValue)
              val cadence = Option(mesg.getField(RecordMesg.CadenceFieldNum)).map(_.getIntegerValue)
              val power = Option(mesg.getField(RecordMesg.PowerFieldNum)).map(_.getIntegerValue)

              for (time <- timestamp) {
                // time may be seconds or miliseconds, how to know?
                val jTime = fromTimestamp(time)
                for (lat <- posLat; long <- posLong) {
                  gpsBuffer += jTime -> decodeLatLng(lat, long, elev)
                }
                for (d <- distance) {
                  distanceBuffer += jTime -> d.toDouble
                }

                for (hr <- heartrate) {
                  hrBuffer += jTime -> hr
                }
                for (v <- power) powerBuffer += jTime -> v
                for (v <- cadence) cadenceBuffer += jTime -> v
              }
            case MesgNum.LAP =>
              val timestamp = Option(mesg.getField(RecordMesg.TimestampFieldNum)).map(_.getLongValue)
              for (time <- timestamp) {
                //println(s"LAP $time")
                lapBuffer += fromTimestamp(time)
              }
            case MesgNum.FILE_ID =>
              val prod = Option(mesg.getField(FileIdMesg.ProductFieldNum))
              val prodName = Option(mesg.getField(FileIdMesg.ProductNameFieldNum))

            case MesgNum.SESSION =>
              // https://strava.github.io/api/v3/uploads/
              // ride, run, swim, workout, hike, walk, nordicski, alpineski, backcountryski, iceskate, inlineskate, kitesurf,
              // rollerski, windsurf, workout, snowboard, snowshoe, ebikeride, virtualride
              val sport = Option(Sport.getByValue(mesg.getField(SessionMesg.SportFieldNum).getShortValue)).map {
                // TODO: handle other sports
                case Sport.CYCLING => Event.Sport.Ride
                case Sport.RUNNING => Event.Sport.Run
                case Sport.HIKING => Event.Sport.Hike
                case Sport.WALKING => Event.Sport.Walk
                case Sport.CROSS_COUNTRY_SKIING => Event.Sport.NordicSki
                case Sport.GENERIC => Event.Sport.Workout
                case Sport.SWIMMING => Event.Sport.Swim
                case _ => Event.Sport.Workout
              }

              if (sport.isDefined) {
                header = header.copy(sport = sport)
              }

            case _ =>

          }
        }
      }

      decode.read(in, listener)

      val gpsStream = SortedMap.from(gpsBuffer)
      val hrStream = SortedMap.from(hrBuffer)
      val powerStream = SortedMap.from(powerBuffer)
      val cadenceStream = SortedMap.from(cadenceBuffer)

      val gpsDataStream = new DataStreamGPS(gpsStream)
      val distData = if (distanceBuffer.nonEmpty) {
        SortedMap.from(distanceBuffer)
      } else {
        val distanceDeltas = gpsDataStream.distStream

        val distances = DataStreamGPS.routeStreamFromDistStream(distanceDeltas.toSeq)

        distances
      }

      val allStreams = Seq(gpsStream, distData, hrStream).filter(_.nonEmpty)
      val startTime = allStreams.map(_.head._1).min
      val endTime = allStreams.map(_.last._1).max

      // TODO: digest
      val id = ActivityId(FilenameId(filename), digest, "Activity", startTime, endTime, header.sport.getOrElse(Event.Sport.Workout), distData.last._2)

      object ImportedStreams extends ActivityEvents.ActivityStreams {

        val dist = new DataStreamDist(distData)

        val latlng = gpsDataStream

        def attributes = Seq() ++
          hrStream.headOption.map(_ => new DataStreamHR(hrStream)) ++
          powerStream.headOption.map(_ => new DataStreamAttrib("watts", powerStream)) ++
          cadenceStream.headOption.map(_ => new DataStreamAttrib("cadence", cadenceStream))
          // TODO: add temperature and other attributes

      }

      Some(ActivityEvents.processActivityStream(id, ImportedStreams, lapBuffer.toSeq, Nil))

    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        None
    } finally {
      in.close()
    }

  }
}

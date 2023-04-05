package com.github.opengrabeso.mixtio

import common.Util._
import common.model._

import java.io.{InputStream, PushbackInputStream}
import java.time.ZonedDateTime
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object TcxImport {
  def apply(filename: String, digest: String, in: InputStream): Try[ActivityEvents] = Try {

    import SAXParser._

    object parsed extends SAXParserWithGrammar {
      var deviceName = Option.empty[String]
      var startTime = Option.empty[ZonedDateTime]
      var sportName = Option.empty[String]
      class Sample {
        /* TCX example:
         <Trackpoint>
          <Time>2017-10-20T09:47:12Z</Time>
          <Position>
           <LatitudeDegrees>49.7970480</LatitudeDegrees>
           <LongitudeDegrees>14.1720700</LongitudeDegrees>
          </Position>
          <AltitudeMeters>394.0</AltitudeMeters>
          <DistanceMeters>4262.7</DistanceMeters>
          <HeartRateBpm>
           <Value>173</Value>
          </HeartRateBpm>
          <Extensions>
           <TPX xmlns="http://www.garmin.com/xmlschemas/ActivityExtension/v2">
            <Speed>4.1</Speed>
           </TPX>
          </Extensions>
         </Trackpoint>
        */
        var time = Option.empty[ZonedDateTime]
        var distance = Option.empty[Double]
        var latitude = Option.empty[Double]
        var longitude = Option.empty[Double]
        var accuracy = Option.empty[Double]
        var elevation: Option[Int] = None
        var heartRate: Option[Int] = None
      }
      val samples = ArrayBuffer.empty[Sample]
      val laps = ArrayBuffer.empty[Lap]

      /**
        * When there is no zone, assume UTC
        * */
      def safeParse(s: String): Try[ZonedDateTime] = {
        Try {
          ZonedDateTime.parse(s)
        }
      }
      def grammar = root(
        "Activities" tag {
          "Activity" tag {
            "Id" text (text => startTime = Try(timeToUTC(ZonedDateTime.parse(text))).toOption)
            "Lap" tag (
              "Track" tag {
                "Trackpoint" tagWithOpen(
                  samples += new Sample,
                  "ele" text (text => samples.last.elevation = Some(text.toDouble.round.toInt)),
                  "Time" text (text => samples.last.time = safeParse(text).toOption),
                  "Position" tag(
                    "LatitudeDegrees" text (text => samples.last.latitude = Some(text.toDouble)),
                    "LongitudeDegrees" text (text => samples.last.longitude = Some(text.toDouble))
                  ),
                  "AltitudeMeters" text (text => samples.last.elevation = Some(text.toDouble.round.toInt)),
                  "DistanceMeters" text (text => samples.last.distance = Some(text.toDouble)),
                  "HeartRateBpm" tag (
                    "Value" text (text => samples.last.heartRate = Some(text.toDouble.round.toInt))
                    )
                )
              }
            ) attrs (
              "StartTime" attr (text => laps.append(Lap("Lap", timeToUTC(ZonedDateTime.parse(text)))))
            )
          } attrs (
            "Sport" attr (text => sportName = Some(text))
          )
        }
      )
    }

    val skipSpaces = new PushbackInputStream(in)
    var lastChar: Int = 0
    do {
      lastChar = skipSpaces.read()
    } while (lastChar == ' ')
    skipSpaces.unread(lastChar)

    SAXParser.parse(skipSpaces)(parsed)

    val gpsSamples = for {
      s <- parsed.samples
      longitude <- s.longitude
      latitude <- s.latitude
      time <- s.time
    } yield {
      time -> GPSPoint(latitude, longitude, s.elevation)(s.accuracy)
    }
    val distSamples = for {
      s <- parsed.samples
      distance <- s.distance
      time <- s.time
    } yield {
      time -> distance
    }
    val hrSamples = for {
      s <- parsed.samples
      v <- s.heartRate if v != 0
      time <- s.time
    } yield {
      time -> v
    }

    val gpsStream = new DataStreamGPS(SortedMap.from(gpsSamples))
    val hrStream = if (hrSamples.exists(_._2 != 0)) Some(new DataStreamHR(SortedMap.from(hrSamples))) else None

    val distData = if (distSamples.nonEmpty) {
      new DataStreamDist(SortedMap.from(distSamples))
    } else {
      new DataStreamDist(DataStreamGPS.routeStreamFromGPS(gpsStream.stream))
    }

    val sport = parsed.sportName.flatMap { s =>
      val condensedName = s.replaceAll("\\s", "")
      Try(Event.Sport.withName(condensedName)).toOption
    }.getOrElse(Event.Sport.Workout)

    val allStreams = Seq(gpsStream, distData) ++ hrStream

    val activity = for {
      startTime <- allStreams.flatMap(_.startTime).minOpt
      endTime <- allStreams.flatMap(_.endTime).maxOpt
    } yield {
      def inRange(t: ZonedDateTime) = t >= startTime && t <= endTime

      val id = ActivityId(FileId.FilenameId(filename), digest, "Activity", startTime, endTime, sport, distData.stream.last._2)

      val events = Array[Event](BegEvent(id.startTime, sport), EndEvent(id.endTime))

      val lapTimes = parsed.laps.map(_.timestamp).filter(inRange)

      val lapEvents = lapTimes.map(LapEvent)

      val allEvents = (events ++ lapEvents).sortBy(_.stamp)

      ActivityEvents(id, allEvents, distData, gpsStream, hrStream.toSeq)
    }

    activity.get
  }
}

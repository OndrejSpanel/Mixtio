package com.github.opengrabeso.mixtio

import common.Util._
import common.model._

import java.io.InputStream
import java.time.ZonedDateTime
import scala.collection.immutable.SortedMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object GpxImport {
  def apply(filename: String, digest: String, in: InputStream): Try[ActivityEvents] = Try {

    import SAXParser._

    object parsed extends SAXParserWithGrammar {
      var rrData = Seq.empty[Int]
      var deviceName = Option.empty[String]
      var startTime = Option.empty[ZonedDateTime]
      var distance: Int = 0
      var durationMs: Int = 0
      var paused: Boolean = false
      var pauseStartTime = Option.empty[ZonedDateTime]
      class Sample {
        /* GPX example:
         <trkpt lat="49.7838710" lon="14.1806550">
          <ele>367.7</ele>
          <time>2017-06-23T08:52:42Z</time>
          <extensions>
           <gpxtpx:TrackPointExtension>
            <gpxtpx:hr>94</gpxtpx:hr>
           </gpxtpx:TrackPointExtension>
          </extensions>
         </trkpt>
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
        "Device" tag ("Name" text (text => deviceName = Some(text))),
        "Metadata" tag (
          "time" text (text => startTime = Some(timeToUTC(ZonedDateTime.parse(text)))),
        ),
        "trk" tag {
          "trkseg" tag {
            "trkpt" tagWithOpen(
              samples += new Sample,
              "ele" text (text => samples.last.elevation = Some(text.toDouble.round.toInt)),
              "time" text (text => samples.last.time = safeParse(text).toOption),
              "extensions" tag (
                "TrackPointExtension" tag (
                  "hr" text (text => samples.last.heartRate = Some(text.toInt))
                )
              )
            ) attrs (
              "lat" attr (text => samples.last.latitude = Some(text.toDouble)),
              "lon" attr (text => samples.last.longitude = Some(text.toDouble))
            )
          }
        }
      )
    }

    SAXParser.parse(in)(parsed)

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

    // TODO: read ActivityType from XML
    val sport = Event.Sport.Workout

    val allStreams = Seq(gpsStream, distData) ++ hrStream

    val activity = for {
      startTime <- allStreams.flatMap(_.startTime).minOpt
      endTime <- allStreams.flatMap(_.endTime).maxOpt
    } yield {

      val id = ActivityId(FileId.FilenameId(filename), digest, "Activity", startTime, endTime, sport, distData.stream.last._2)

      val events = Array[Event](BegEvent(id.startTime, sport), EndEvent(id.endTime))

      // TODO: avoid duplicate timestamp events
      val lapEvents = Nil // lapTimes.map(LapEvent)

      val allEvents = (events ++ lapEvents).sortBy(_.stamp)

      ActivityEvents(id, allEvents, distData, gpsStream, hrStream.toSeq)
    }

    activity.get
  }
}

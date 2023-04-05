package com.github.opengrabeso.mixtio
package moveslink2


import java.io._
import java.time.{ZoneId, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter

import Main._
import common.model._

import scala.collection.immutable.SortedMap
import common.Util._

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object XMLParser {
  private val PositionConstant = 57.2957795131

  private val dateFormatNoZone = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").withZone(ZoneId.systemDefault)
  private val dateFormatNoZoneUTC = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").withZone(ZoneOffset.UTC)

  def getRRArray(rrData: String): Seq[Int] = {
    val rrArray = rrData.split(" ")
    for (rr <- rrArray) yield rr.toInt
  }

  def parseXML(fileName: String, inputStream: InputStream, digest: String): Option[ActivityEvents] = {

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
        /* GPS Track Pod example:
        <Sample>
          <Latitude>0.86923005364868888</Latitude>
          <Longitude>0.24759951117797119</Longitude>
          <GPSAltitude>416</GPSAltitude>
          <GPSHeading>1.4116222990130136</GPSHeading>
          <EHPE>4</EHPE>
          <Time>2534</Time>
          <UTC>2016-10-21T07:28:14Z</UTC>
        </Sample>
        <Sample>
          <VerticalSpeed>0</VerticalSpeed>
          <Distance>7868</Distance>
          <Speed>3.9399999999999999</Speed>
          <Time>2534.6120000000001</Time>
          <SampleType>periodic</SampleType>
          <UTC>2016-10-21T07:28:14.612Z</UTC>
        </Sample>
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
        } orElse Try {
          ZonedDateTime.parse(s, dateFormatNoZoneUTC)
        }
      }
      def grammar = root(
        "Device" tag ("Name" text (text => deviceName = Some(text))),
        "Header" tag (
          "Distance" text (text => distance = text.toInt),
          // caution: GPS Track POD <Header>/<DateTime> is given in local timezone with no designation - better ignore it
          "DateTime" text (text => startTime = Some(timeToUTC(ZonedDateTime.parse(text, dateFormatNoZone)))),
          "Duration" text (text => durationMs = (text.toDouble * 1000).toInt)
        ),
        "R-R" tag ("Data" text (text => rrData = getRRArray(text))),
        "Samples" tag (
          "Sample" tagWithOpen (
            samples += new Sample,
            "Latitude" text (text => samples.last.latitude = Some(text.toDouble * XMLParser.PositionConstant)),
            "Longitude" text (text => samples.last.longitude = Some(text.toDouble * XMLParser.PositionConstant)),
            "GPSAltitude" text (text => samples.last.elevation = Some(text.toInt)),
            "EHPE" text (text => samples.last.accuracy = Some(text.toInt)),
            // TODO: handle relative time when UTC is not present
            "UTC" text (text => samples.last.time = safeParse(text).toOption),
            "Distance" text (text => samples.last.distance = Some(text.toDouble)),
            "HR" text (text => samples.last.heartRate = Some(text.toInt)),
            // TODO: add other properties (power, cadence, temperature ...)

            "Events" tag (
              "Pause" tag ("State" text (text => paused = text.equalsIgnoreCase("true"))),
              "Lap" tagWithOpen {
                // caution: lap time is bad for GPS Track Pod. It is marked as <UTC>, but in fact it is written in local time zone
                // use time of the last previous sample instead, or we might consider using duration inside of the lap event
                val lastTime = samples.reverseIterator.toIterable.tail.find(_.time.isDefined).flatMap(_.time)
                for (timestamp <- lastTime) {
                  laps += Lap("Lap", timestamp)
                  println(s"SML lap $timestamp")
                }
              }
              //"Type" text { text =>}
              // we are not interested about any other Lap properties
              //"Duration" text { text => text.toDouble }
              //"Duration" text {text => }
              //"Distance" text {text => }
            )
          )
        )
      )
    }

    SAXParser.parse(inputStream)(parsed)

    // always check time last, as this is present in almost each entry. We want first check to filter out as much as possible
    val gpsSamples = for {
      s <- parsed.samples
      longitude <- s.longitude
      latitude <- s.latitude
      time <- s.time
    } yield {
      time -> GPSPoint(latitude, longitude, s.elevation)(s.accuracy)
    }

    val ret = for (gpsInterestingRange <- DataStreamGPS.dropAlmostEmpty(gpsSamples.toList)) yield {

      def inRange(t: ZonedDateTime) = t >= gpsInterestingRange._1 && t <= gpsInterestingRange._2

      val distSamples = for {
        s <- parsed.samples
        distance <- s.distance
        time <- s.time if inRange(time)
      } yield {
        time -> distance
      }
      val hrSamples = for {
        s <- parsed.samples
        v <- s.heartRate if v != 0
        time <- s.time if inRange(time)
      } yield {
        time -> v
      }


      val gpsStream = new DataStreamGPS(SortedMap.from(gpsSamples.filter(s => inRange(s._1))))
      val distStream = new DataStreamDist(SortedMap.from(distSamples))

      val hrStream = if (hrSamples.exists(_._2 != 0)) Some(new DataStreamHR(SortedMap.from(hrSamples))) else None

      val lapTimes = parsed.laps.map(_.timestamp).filter(inRange)

      // TODO: read ActivityType from XML
      val sport = Event.Sport.Workout

      val allStreams = Seq(distStream, gpsStream) ++ hrStream

      val activity = for {
        startTime <- allStreams.flatMap(_.startTime).minOpt
        endTime <- allStreams.flatMap(_.endTime).maxOpt
        d <- distStream.stream.lastOption.map(_._2)
      } yield {

        val id = ActivityId(FileId.FilenameId(fileName), digest, "Activity", startTime, endTime, sport, d)

        val events = Array[Event](BegEvent(id.startTime, sport), EndEvent(id.endTime))

        // TODO: avoid duplicate timestamp events
        val lapEvents = lapTimes.map(LapEvent)

        val allEvents = (events ++ lapEvents).sortBy(_.stamp)

        ActivityEvents(id, allEvents, distStream, gpsStream, hrStream.toSeq)
      }
      activity
    }


    ret.flatten

  }

}

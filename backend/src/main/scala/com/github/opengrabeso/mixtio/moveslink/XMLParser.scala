package com.github.opengrabeso.mixtio
package moveslink

import java.io.{InputStream, PushbackInputStream}

import java.time.{ZonedDateTime, _}
import java.time.format.DateTimeFormatter

import common.Util._

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object XMLParser {
  private val dateFormatBase = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  private def dateFormatWithZone(timezone: String) = dateFormatBase.withZone(ZoneId.of(timezone))


  def parseTime(timeText: String, timezone: String): ZonedDateTime = {
    timeToUTC(ZonedDateTime.parse(timeText, dateFormatWithZone(timezone)))
  }

  def parseDuration(timeStr: String): Duration = {
    val relTime = LocalTime.parse(timeStr)
    val ns = relTime.toNanoOfDay
    Duration.ofNanos(ns)
  }

  def skipMoveslinkDoctype(is: InputStream): InputStream = {
    val pbStream =  new PushbackInputStream(is, 100)
    val wantedPrefix = """<?xml version="1.0" encoding="ISO-8859-1"?>"""
    val prefixToRemove = """<!DOCTYPE xml>"""

    @scala.annotation.tailrec
    def skipPrefix(p: List[Byte]): Boolean = {
      if (p.isEmpty) true
      else {
        val c = pbStream.read()
        if (c == p.head) skipPrefix(p.tail)
        else {
          pbStream.unread(c)
          false
        }
      }
    }
    @scala.annotation.tailrec
    def skipEmptyLines(): Unit = {
      val c = pbStream.read().toChar
      if (c.isWhitespace) skipEmptyLines()
      else pbStream.unread(c)
    }

    val wantedPresent = skipPrefix(wantedPrefix.getBytes.toList)
    skipEmptyLines()
    skipPrefix(prefixToRemove.getBytes.toList)
    skipEmptyLines()

    if (wantedPresent) {
      pbStream.unread(wantedPrefix.getBytes)
    }
    pbStream
  }

  def parseXML(fileName: String, document: InputStream, timezone: String): Seq[Move] = {

    import SAXParser._
    object parsed extends SAXParserWithGrammar {
      var deviceName = Option.empty[String]

      class Move {
        //var calories = Option.empty[Int]
        //var distance = Option.empty[Int]
        var startTime = Option.empty[ZonedDateTime]
        var durationMs: Int = 0
        var activityType: MoveHeader.ActivityType = MoveHeader.ActivityType.Unknown
        var lapDurations = ArrayBuffer.empty[Duration]
        var distanceSamples = Seq.empty[Double]
        var heartRateSamples = Seq.empty[Int]
      }

      val moves = ArrayBuffer.empty[Move]

      def grammar = root(
        "Device" tag (
          "FullName" text (text => parsed.deviceName = Some(text))
        ),
        "Move" tagWithOpen (
          moves += new Move,
          "Header" tag (
            "Duration" text {text =>
              val DurationPattern = "(\\d+):(\\d+):(\\d+)\\.?(\\d*)".r
              object Int {
                // handle s == null correctly, handles missing milliseconds
                def unapply(s : String) : Option[Int] = Option(s).map {
                  case "" => 0
                  case x => x.toInt
                }
              }

              val duration: Int = text match {
                case DurationPattern(Int(hour), Int(minute), Int(second), Int(ms)) =>
                  (hour * 3600 + minute * 60 + second) * 1000 + ms
                case _ => 0
              }
              moves.last.durationMs = duration
            },
            "Time" text { text =>
              val startTime = parseTime(text, timezone)
              moves.last.startTime = Some(startTime)
            },
            "Activity" text {text =>
              import MoveHeader.ActivityType._
              val sportType = Try(text.toInt).getOrElse(0)
              // TODO: add at least most common sports
              val activityType = sportType match {
                case 82 => RunningTrail
                case 75 => Orienteering
                case 5 => MountainBike
                case _ => Unknown
              }
              moves.last.activityType = activityType
            }
            /* never used, no need to parse
            case _ / "Move" / "Header" / "Calories" =>
              moves.last.calories = Some(text.toInt)
            case _ / "Move" / "Header" / "Distance" =>
              moves.last.distance = Some(text.toInt)
            */

          ),
          "Samples" tag (
            "Distance" text {text =>
              moves.last.distanceSamples = text.split(" ").dropWhile(_ == "").scanLeft(0.0)(_ + _.toDouble)
            },
            "HR" text {text =>
              def duplicateHead(strs: Seq[String]) = {
                if (strs.head.isEmpty) strs.tail.head +: strs.tail
                else strs
              }

              moves.last.heartRateSamples = duplicateHead(text.split(" ")).map(_.toInt)
            }
            // TODO: Cadence, Power, Temperature ...
          ),
          "Marks" tag (
            "Mark" tag (
              "Time" text (text => moves.last.lapDurations appendAll Try(parseDuration(text)).toOption)
            )
          )
        )

      )
    }

    parse(document)(parsed)

    for (i <- parsed.moves.indices) yield {
      val mi = parsed.moves(i)

      val timeRange = 0 until mi.durationMs by 10000

      def timeMs(ms: Int) = mi.startTime.get.plus(Duration.ofMillis(ms))

      val timedMapHR = (timeRange zip mi.heartRateSamples).collect { case (t, s) if s > 0 =>
        timeMs(t) -> s
      }

      val timedMapDist = (timeRange zip mi.distanceSamples).collect { case (t, d) =>
        timeMs(t) -> d
      }

      val header = new MoveHeader(parsed.deviceName.toSet, mi.activityType)
      val hrStream = new DataStreamHR(SortedMap(timedMapHR: _*))
      val distStream = new DataStreamDist(SortedMap(timedMapDist: _*))

      val laps = mi.lapDurations.scanLeft(mi.startTime.get) { (time, duration) => time.plus(duration)}

      val move = new Move(Set(fileName), header, hrStream, distStream)
      if (laps.nonEmpty) {
        move.addStream(move, new DataStreamLap(SortedMap.from(laps.map(time => time -> "Manual"))))
      } else {
        move
      }
    }

  }

}

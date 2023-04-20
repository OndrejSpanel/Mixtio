package com.github.opengrabeso.mixtio

import java.time.temporal.ChronoUnit
import com.garmin.fit
import com.garmin.fit.{Event => FitEvent, _}
import common.Util._
import common.model._

import java.time.ZonedDateTime

object FitExport {
  type Encoder = MesgListener with MesgDefinitionListener

  private def createEncoder: BufferEncoder = {
    new BufferEncoder(Fit.ProtocolVersion.V1_0)
  }

  def encodeHeader(encoder: Encoder): Unit = {
    //Generate FileIdMessage
    val fileIdMesg = new FileIdMesg
    fileIdMesg.setType(fit.File.ACTIVITY)
    encoder.onMesg(fileIdMesg)
  }

  def toTimestamp(zonedTime: ZonedDateTime): DateTime = {
    val instant = zonedTime.toInstant
    val timestamp = instant.toEpochMilli / 1000 - DateTime.OFFSET / 1000.0
    val dateTime = new DateTime(0, timestamp)
    dateTime
  }

  def `export`(events: ActivityEvents): Array[Byte] = {
    val encoder = createEncoder

    abstract class FitEvent {
      def time: ZonedDateTime

      def encode(encoder: Encoder): Unit
    }

    abstract class DataEvent(time: ZonedDateTime, set: RecordMesg => Unit) extends FitEvent {
      override def encode(encoder: Encoder): Unit = {
        val myMsg = new RecordMesg()
        myMsg.setTimestamp(toTimestamp(time))
        set(myMsg)
        encoder.onMesg(myMsg)
      }
    }

    def encodeGPS(msg: RecordMesg, gps: GPSPoint) = {
      val longLatScale = (1L << 31).toDouble / 180
      msg.setPositionLong((gps.longitude * longLatScale).toInt)
      msg.setPositionLat((gps.latitude * longLatScale).toInt)
      gps.elevation.foreach(e => msg.setAltitude(e.toFloat))

    }

    class GPSEvent(val time: ZonedDateTime, val gps: GPSPoint) extends DataEvent(time, encodeGPS(_, gps))

    class AttribEvent(val time: ZonedDateTime, data: Int, set: (RecordMesg, Int) => Unit) extends DataEvent(time, set(_, data))

    val gpsAsEvents = events.gps.stream map { case (t, gps) =>
      new GPSEvent(t, gps)
    }


    val attributesWithLast = events.attributes.map { attr =>
      attr.stream
    }

    val attributesAsEvents = events.attributes.flatMap { attrib =>
      val createAttribEvent: (RecordMesg, Int) => Unit = (msg, value) =>
        attrib match {
          case x: DataStreamHR => msg.setHeartRate(value.toShort)
          case x: DataStreamAttrib =>
            x.attribName match {
              case "watts" => msg.setPower(value)
              case "cadence" => msg.setCadence(value.toShort)
              case "temp" => msg.setTemperature(value.toByte)
              case _ => // unsupported attribute
            }
          case _ => ???
        }
      val attribStream = if (false) {
        // attempt to fix Strava not showing temperature: make sure each attribute is present for the last GPS value
        val lastGPSTime = events.gps.stream.lastKey
        if (attrib.stream contains lastGPSTime) {
          attrib.stream
        } else {
          attrib.stream ++ attrib.stream.until(lastGPSTime).lastOption.map(lastGPSTime -> _._2)
        }
      } else attrib.stream

      attribStream.map { case (t, data) =>
        new AttribEvent(t, data.asInstanceOf[Int], createAttribEvent)
      }
    }

    trait AutoClose {
      def emitMsg(time: ZonedDateTime, endTime: ZonedDateTime): Unit

      private var isOpen = false
      private var counter = 0
      private var lastStart = events.id.startTime

      def count: Int = counter

      def openLap(time: ZonedDateTime): Unit = {
        lastStart = time
        isOpen = true
      }
      def closeLap(time: ZonedDateTime): Unit = {
        if (isOpen && time > lastStart) {
          emitMsg(lastStart, time)
          counter += 1
        }
        openLap(time)
      }
    }
    object LapAutoClose extends AutoClose {
      def emitMsg(startTime: ZonedDateTime, endTime: ZonedDateTime): Unit = {
        val myMsg = new LapMesg()
        myMsg.setEvent(FitEvent.LAP)
        myMsg.setEventType(EventType.STOP)
        myMsg.setStartTime(toTimestamp(startTime))
        myMsg.setTimestamp(toTimestamp(endTime))
        myMsg.setMessageIndex(count)
        val lapDurationSec = ChronoUnit.SECONDS.between(startTime, endTime).toFloat
        myMsg.setTotalElapsedTime(lapDurationSec)
        myMsg.setTotalTimerTime(lapDurationSec)
        encoder.onMesg(myMsg)
      }
    }

    def closeActivity(timeEnd: ZonedDateTime): Unit = {
      val myMsg = new ActivityMesg()
      myMsg.setTimestamp(toTimestamp(timeEnd))
      myMsg.setNumSessions(1)
      myMsg.setType(Activity.MANUAL)
      myMsg.setEvent(FitEvent.ACTIVITY)
      myMsg.setEventType(EventType.STOP)
      encoder.onMesg(myMsg)
    }


    class LapFitEvent(val time: ZonedDateTime) extends FitEvent {
      override def encode(encoder: Encoder): Unit = {
        LapAutoClose.closeLap(time)
      }
    }

    val lapsAsEvents = events.events.collect {
      case LapEvent(time) =>
        new LapFitEvent(time)
    }

    val allEvents = (gpsAsEvents ++ attributesAsEvents ++ lapsAsEvents).toVector.sortBy(_.time)

    val timeBeg = allEvents.head.time
    val timeEnd = allEvents.last.time

    def encodeHeader(encoder: Encoder): Unit = {
      //Generate FileIdMessage
      val fileIdMesg = new FileIdMesg
      fileIdMesg.setManufacturer(Manufacturer.SUUNTO)
      fileIdMesg.setType(fit.File.ACTIVITY)
      fileIdMesg.setProduct(1) // TODO: detect for real
      fileIdMesg.setTimeCreated(toTimestamp(timeBeg))
      encoder.onMesg(fileIdMesg)
    }

    encodeHeader(encoder)

    LapAutoClose.openLap(timeBeg)
    allEvents.foreach(_.encode(encoder))

    val durationSec = ChronoUnit.SECONDS.between(timeBeg, timeEnd)

    LapAutoClose.closeLap(timeEnd)

    val (sport, subsport) = events.id.sportName match {
      // TODO: handle other sports
      case Event.Sport.Run => (Sport.RUNNING, SubSport.GENERIC)
      case Event.Sport.Ride => (Sport.CYCLING, SubSport.GENERIC)
      case Event.Sport.Swim => (Sport.SWIMMING, SubSport.GENERIC)
      case Event.Sport.Hike => (Sport.HIKING, SubSport.GENERIC)
      case Event.Sport.Walk => (Sport.WALKING, SubSport.GENERIC)
      case Event.Sport.NordicSki => (Sport.CROSS_COUNTRY_SKIING, SubSport.GENERIC)
      case Event.Sport.AlpineSki => (Sport.ALPINE_SKIING, SubSport.GENERIC)
      case Event.Sport.Canoeing => (Sport.PADDLING, SubSport.GENERIC)
      case Event.Sport.Rowing => (Sport.ROWING, SubSport.GENERIC)
      case Event.Sport.Surfing => (Sport.SURFING, SubSport.GENERIC)
      case Event.Sport.IceSkate => (Sport.ICE_SKATING, SubSport.GENERIC)
      case Event.Sport.InlineSkate => (Sport.INLINE_SKATING, SubSport.GENERIC)
      case Event.Sport.Kayaking => (Sport.KAYAKING, SubSport.GENERIC)
      case Event.Sport.WindSurf => (Sport.WINDSURFING, SubSport.GENERIC)
      case Event.Sport.KiteSurf => (Sport.KITESURFING, SubSport.GENERIC)
      case Event.Sport.Snowshoe => (Sport.SNOWSHOEING, SubSport.GENERIC)
      case Event.Sport.EbikeRide => (Sport.E_BIKING, SubSport.GENERIC)
      //case Event.Sport.WindSurfing => (Sport.SAILING, SubSport.GENERIC)
      case _ => (Sport.GENERIC, SubSport.GENERIC)
    }

    {
      val myMsg = new SessionMesg()
      myMsg.setStartTime(toTimestamp(timeBeg))
      myMsg.setTimestamp(toTimestamp(timeEnd))
      myMsg.setSport(sport)
      myMsg.setSubSport(subsport)
      myMsg.setTotalElapsedTime(durationSec.toFloat)
      myMsg.setTotalTimerTime(durationSec.toFloat)
      myMsg.setMessageIndex(0)
      myMsg.setFirstLapIndex(0)
      myMsg.setNumLaps(LapAutoClose.count + 1)

      myMsg.setEvent(FitEvent.SESSION)
      myMsg.setEventType(EventType.STOP)

      encoder.onMesg(myMsg)
    }

    closeActivity(timeEnd)

    encoder.close
  }


}

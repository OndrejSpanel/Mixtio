package com.github.opengrabeso.mixtio
package common.model

import java.time.ZonedDateTime

import rest.EnhancedRestDataCompanion

case class EventKind(id: String, display: String)

object Event extends EnhancedRestDataCompanion[Event] {
  final type Sport = SportId.Value
  final val Sport = SportId

  // lower priority means more preferred
  def sportPriority(sport: Sport): Int = sport.id
}

object EventPriority {
  val seq: IndexedSeq[Class[_]] = IndexedSeq(
    classOf[BegEvent], classOf[EndEvent],
    classOf[SplitEvent],
    classOf[StartSegEvent], classOf[EndSegEvent],
    classOf[PauseEvent], classOf[PauseEndEvent],
    classOf[LapEvent],
    classOf[ElevationEvent]
  )

  def apply(e: Event) = {
    val find = seq.indexOf(e.getClass)
    //println(s"Find order of ${e.getClass} as $find")
    if (find<0) seq.size // not listed means lowest possible priority
    else find
  }
}


@SerialVersionUID(10)
sealed abstract class Event {

  import Event._

  def stamp: ZonedDateTime
  def isSplit: Boolean // splits need to be known when exporting
  def sportChange: Option[Event.Sport] = None
  lazy val order: Int = EventPriority(this)// higher order means less important
  def timeOffset(offset: Int): Event

  def defaultEvent: String
  def originalEvent: String = defaultEvent

  protected def listSplitTypes: Seq[EventKind] = {
    Sport.values.toSeq.map { s =>
      val sport = s.toString
      EventKind(s"split$sport", s"- $sport")
    }
  }

  def listTypes: Array[EventKind] = (Seq(
    EventKind("", "--"),
    EventKind("lap", "Lap")
  ) ++ listSplitTypes).toArray
}

object Events {

  def typeToDisplay(listTypes: Array[EventKind], name: String): String = {
    listTypes.find(_.id == name).map(_.display).getOrElse("")
  }

  def niceDuration(duration: Int): String = {
    def round(x: Int, div: Int) = (x + div / 2) / div * div
    val minute = 60
    if (duration < minute) {
      s"${round(duration, 5)} sec"
    } else {
      val minutes = duration / minute
      val seconds = duration - minutes * minute
      if (duration < 5 * minute) {
        f"$minutes:${round(seconds, 10)}%2d min"
      } else {
        s"$minutes min"
      }
    }
  }

}

@SerialVersionUID(10)
case class PauseEvent(duration: Int, stamp: ZonedDateTime) extends Event {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset)) // Find some way to DRY
  def defaultEvent = if (duration >= 30) "lap" else ""
  override def originalEvent = if (duration >= 50) "long pause" else "pause"
  def isSplit = false
}
@SerialVersionUID(10)
case class PauseEndEvent(duration: Int, stamp: ZonedDateTime) extends Event {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset))
  def defaultEvent = if (duration >= 50) "lap" else ""
  override def originalEvent = if (duration >= 50) "long pause end" else "pause end"
  def isSplit = false
}
@SerialVersionUID(10)
case class LapEvent(stamp: ZonedDateTime) extends Event {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset))
  def defaultEvent = "lap"
  def isSplit = false
}

@SerialVersionUID(10)
case class EndEvent(stamp: ZonedDateTime) extends Event {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset))
  def defaultEvent = "end"
  def isSplit = true

  override def listTypes: Array[EventKind] = Array(EventKind("", "--"))
}

@SerialVersionUID(10)
case class BegEvent(stamp: ZonedDateTime, sport: Event.Sport) extends Event {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset))
  def defaultEvent = s"split${sport.toString}"
  def isSplit = true
  override def sportChange: Option[Event.Sport] = Some(sport)

  override def listTypes = listSplitTypes.toArray
}

@SerialVersionUID(10)
case class SplitEvent(stamp: ZonedDateTime, sport: Event.Sport) extends Event {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset))
  def defaultEvent = s"split${sport.toString}"
  def isSplit = true
  override def sportChange: Option[Event.Sport] = Some(sport)
}

trait SegmentTitle {
  def isPrivate: Boolean
  def segmentId: Long
  def name: String
}

@SerialVersionUID(11)
case class StartSegEvent(name: String, isPrivate: Boolean, segmentId: Long, stamp: ZonedDateTime) extends Event with SegmentTitle {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset))
  def defaultEvent = ""
  override def originalEvent = "segment beg"
  def isSplit = false
}
@SerialVersionUID(11)
case class EndSegEvent(name: String, isPrivate: Boolean, segmentId: Long, stamp: ZonedDateTime) extends Event with SegmentTitle {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset))
  def defaultEvent = ""
  override def originalEvent = "segment end"
  def isSplit = false
}

@SerialVersionUID(10)
case class ElevationEvent(elev: Double, stamp: ZonedDateTime) extends Event {
  def timeOffset(offset: Int) = copy(stamp = stamp.plusSeconds(offset))
  def defaultEvent = ""
  override def originalEvent = "elevation"
  def isSplit = false
}

case class EditableEvent(var action: String, time: Int, km: Double, kinds: Array[EventKind], var actionOriginal: String, actionDescription: String) {
  override def toString: String = {
    //val select = ActivityRequest.htmlSelectEvent(time.toString, kinds, action)
    //val selectHtmlSingleLine = select.toString.lines.mkString(" ")

    val description = s"""${common.Formatting.displaySeconds(time)} ${common.Formatting.displayDistance(km)}<br />""" + actionDescription
    s""""$action", '$time', '$km', '$description', "$actionOriginal""""
  }
}

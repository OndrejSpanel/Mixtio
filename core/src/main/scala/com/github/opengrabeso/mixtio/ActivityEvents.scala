package com.github.opengrabeso.mixtio

import common.Util._
import common.model._
import shared.Timing
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.util.control.Breaks.{break, breakable}

object ActivityEvents {
  def mergeAttributes(thisAttributes: Seq[DataStreamAttrib], thatAttributes: Seq[DataStreamAttrib]): Seq[DataStreamAttrib] = {
    val mergedAttr = thisAttributes.map { a =>
      val aThat = thatAttributes.find(t => t.streamType == a.streamType && t.attribName == a.attribName)
      val aStream = aThat.map(a.stream ++ _.stream).getOrElse(a.stream)
      a.pickData(aStream)
    }
    val notMergedFromThat = thatAttributes.find(ta => !thisAttributes.exists(_.streamType == ta.streamType))
    mergedAttr ++ notMergedFromThat
  }


  trait ActivityStreams {
    def dist: DataStreamDist

    def latlng: DataStreamGPS

    def attributes: Seq[DataStreamAttrib]
  }

  def detectSportBySpeed(stats: SpeedStats, defaultName: Event.Sport) = {
    def detectSport(maxRun: Double, fastRun: Double, medianRun: Double): Event.Sport = {
      if (stats.median <= medianRun && stats.fast <= fastRun && stats.max <= maxRun) Event.Sport.Run
      else Event.Sport.Ride
    }

    def paceToKmh(pace: Double) = 60 / pace

    def kmh(speed: Double) = speed

    val sport = defaultName match {
      case Event.Sport.Run =>
        // marked as run, however if clearly contradicting evidence is found, make it a ride
        detectSport(paceToKmh(2), paceToKmh(2.5), paceToKmh(3)) // 2 - 3 min/km possible
      case Event.Sport.Ride =>
        detectSport(kmh(20), kmh(17), kmh(15)) // 25 - 18 km/h possible
      case Event.Sport.Workout =>
        detectSport(paceToKmh(3), paceToKmh(4), paceToKmh(4)) // 3 - 4 min/km possible
      case s => s
    }
    sport
  }

  def findHills(latlng: DataStreamGPS, dist: DataStreamDist#DataMap): Seq[Event] = {
    // find global min and max
    if (latlng.stream.isEmpty) {
      Seq.empty
    } else {
      val routeDist = DataStreamGPS.routeStreamFromDistStream(dist.toSeq)

      case class ElevDist(stamp: ZonedDateTime, elev: Int, dist: Double)
      val elevStream = latlng.stream.toList.flatMap { case (stamp, gps) =>
        gps.elevation.map(e => ElevDist(stamp, e, routeDist(stamp)))
      }
      val max = elevStream.maxBy(_.elev)
      val min = elevStream.minBy(_.elev)
      val minimalHillHeight = 5
      if (max.elev > min.elev + minimalHillHeight) {
        val globalOnly = false

        if (globalOnly) {
          Seq(
            ElevationEvent(max.elev, max.stamp),
            ElevationEvent(min.elev, min.stamp)
          )
        } else {

          // find all local extremes

          // get rid of monotonous rise/descends
          def removeMidSlopes(todo: List[ElevDist], done: List[ElevDist]): List[ElevDist] = {
            todo match {
              case a0 :: a1 :: a2 :: tail =>
                if (a0.elev <= a1.elev && a1.elev <= a2.elev || a0.elev >= a1.elev && a1.elev >= a2.elev) {
                  removeMidSlopes(a0 :: a2 :: tail, done)
                } else {
                  removeMidSlopes(a1 :: a2 :: tail, a0 :: done)
                }
              case _ =>
                done.reverse
            }
          }


          def filterSlopes(input: List[ElevDist]): List[ElevDist] = {

            var todo = input
            breakable {
              while (todo.lengthCompare(2) > 0) {
                // find min elevation difference
                // removing this never shortens slope

                val elevPairs = todo zip todo.drop(1).map(_.elev)
                val elevDiff = elevPairs.map { case (ed, elev) => ed.stamp -> (ed.elev - elev).abs }

                val minElevDiff = elevDiff.minBy(_._2)

                // the less samples we have, the more
                // with 2 samples we ignore 15 meters
                // with 10 samples we ignore 75 meters
                // with 20 samples we ignore 150 meters

                val neverIgnoreElevCoef = 7.5
                if (minElevDiff._2 > todo.length * neverIgnoreElevCoef) break()

                val locate = todo.indexWhere(_.stamp == minElevDiff._1)

                todo = todo.patch(locate, Nil, 2)
              }

            }
            todo
          }

          val slopes = removeMidSlopes(elevStream, Nil)

          //val slopesElev = slopes.map(_.elev)

          //val totalElev = (slopesElev zip slopesElev.drop(1)).map { case (a,b) => (a-b).abs }.sum
          //val minMaxDiff = max.elev - min.elev

          val filteredSlopes = filterSlopes(slopes)

          filteredSlopes.map(x => ElevationEvent(x.elev, x.stamp))
        }
      } else {
        Seq.empty
      }
    }
  }


  def processActivityStream(actId: ActivityId, act: ActivityEvents.ActivityStreams, laps: Seq[ZonedDateTime], segments: Seq[Event]): ActivityEvents = {

    //println(s"Raw laps $laps")
    val cleanLaps = laps.filter(l => l > actId.startTime && l < actId.endTime)

    //println(s"Clean laps $cleanLaps")

    val events = (BegEvent(actId.startTime, actId.sportName) +: EndEvent(actId.endTime) +: cleanLaps.map(LapEvent)) ++ segments

    val eventsByTime = events.sortBy(_.stamp)

    ActivityEvents(actId, eventsByTime.toArray, act.dist, act.latlng, act.attributes)
  }

}

import ActivityEvents._

@SerialVersionUID(10L)
case class ActivityEvents(id: ActivityId, events: Array[Event], dist: DataStreamDist, gps: DataStreamGPS, attributes: Seq[DataStreamAttrib]) {
  self =>



  import ActivityEvents._

  def computeDistStream = {
    if (gps.stream.nonEmpty) {
      gps.distStream
    } else {
      DataStreamGPS.distStreamFromRouteStream(dist.stream.toSeq)
    }
  }

  def computeSpeedStats: SpeedStats = DataStreamGPS.speedStats(DataStreamGPS.computeSpeedStream(computeDistStream))

  def header: ActivityHeader = ActivityHeader(id, hasGPS, hasAttributes, computeSpeedStats)

  def streams: Seq[DataStream] = {
    Seq(dist, gps).filter(_.nonEmpty) ++ attributes
  }

  def startTime = id.startTime
  def endTime = id.endTime
  def duration: Double = timeDifference(startTime, endTime)

  def isAlmostEmpty(minDurationSec: Int) = {
    val ss = streams
    !ss.exists(_.stream.nonEmpty) || endTime < startTime.plusSeconds(minDurationSec) || ss.exists(x => x.isAlmostEmpty)
  }

  override def toString = id.toString
  def toLog: String = streams.map(_.toLog).mkString(", ")

  assert(events.forall(_.stamp >= id.startTime))
  assert(events.forall(_.stamp <= id.endTime))

  assert(events.forall(_.stamp <= id.endTime))

  assert(gps.inTimeRange(id.startTime, id.endTime))
  assert(dist.inTimeRange(id.startTime, id.endTime))
  assert(attributes.forall(_.inTimeRange(id.startTime, id.endTime)))

  def secondsInActivity(time: ZonedDateTime): Int  = id.secondsInActivity(time)
  def timeInActivity(seconds: Int) = id.timeInActivity(seconds)

  private def convertGPSToPair(gps: GPSPoint) = (gps.latitude, gps.longitude)

  def begPos: (Double, Double) = convertGPSToPair(gps.stream.head._2)
  def endPos: (Double, Double) = convertGPSToPair(gps.stream.last._2)

  // must call hasGPS because it is called while composing the JS, even when hasGPS is false
  def lat: Double = if (hasGPS) (begPos._1 + endPos._1) * 0.5 else 0.0
  def lon: Double = if (hasGPS) (begPos._2 + endPos._2) * 0.5 else 0.0

  def hasGPS: Boolean = gps.nonEmpty
  def hasAttributes: Boolean = attributes.exists(_.stream.nonEmpty)

  def distanceForTime(time: ZonedDateTime): Double = dist.distanceForTime(time)

  lazy val elevation: Double = {
    val elevationStream = gps.stream.flatMap {
      case (k, v) =>
        v.elevation.map(k -> _.toDouble)
    }
    val elevations = elevationStream.values
    (elevations zip elevations.drop(1)).map {case (prev, next) => (next - prev) max 0}.sum
  }

  def eventTimes: DataStream.EventTimes = events.map(_.stamp).toList

  def merge(that: ActivityEvents): ActivityEvents = {
    // select some id (name, sport ...)
    val begTime = Seq(id.startTime, that.id.startTime).min
    val endTime = Seq(id.endTime, that.id.endTime).max

    // TODO: unique ID (merge or hash input ids?)
    val sportName = if (Event.sportPriority(id.sportName) < Event.sportPriority(that.id.sportName)) id.sportName else that.id.sportName

    val eventsAndSports = (events ++ that.events).sortBy(_.stamp)

    // keep only first start Event, change other to Split only
    val (begs, others) = eventsAndSports.partition(_.isInstanceOf[BegEvent])
    val (ends, rest) = others.partition(_.isInstanceOf[EndEvent])

    val begsSorted = begs.sortBy(_.stamp).map(_.asInstanceOf[BegEvent])
    val begsAdjusted = begsSorted.take(1) ++ begsSorted.drop(1).map(e => SplitEvent(e.stamp, e.sport))

    // when activities follow each other, insert a lap or a pause between them
    val begsEnds = (begs ++ ends).sortBy(_.stamp)

    val pairs = begsEnds zip begsEnds.drop(1)
    val transitionEvents: Seq[Event] = pairs.flatMap {
      case (e: EndEvent, b: BegEvent) =>
        val duration = timeDifference(e.stamp, b.stamp).toInt
        if (duration < 60) {
          Seq(LapEvent(e.stamp), LapEvent(b.stamp))
        } else {
          Seq(PauseEvent(duration, e.stamp), PauseEndEvent(duration, b.stamp))
        }
      case _ =>
        Seq.empty
    }

    val eventsAndSportsSorted = (begsAdjusted ++ rest ++ transitionEvents :+ ends.maxBy(_.stamp) ).sortBy(_.stamp)

    val startBegTimes = Seq(this.startTime, this.endTime, that.startTime, that.endTime).sorted

    val timeIntervals = startBegTimes zip startBegTimes.tail

    val streams = for (timeRange <- timeIntervals) yield {
      // do not merge overlapping distances, prefer distance from a GPS source
      val thisGpsPart = this.gps.slice(timeRange._1, timeRange._2)
      val thatGpsPart = that.gps.slice(timeRange._1, timeRange._2)

      val thisDistPart = this.dist.slice(timeRange._1, timeRange._2)
      val thatDistPart = that.dist.slice(timeRange._1, timeRange._2)

      val thisAttrPart = this.attributes.map(_.slice(timeRange._1, timeRange._2))
      val thatAttrPart = that.attributes.map(_.slice(timeRange._1, timeRange._2))

      (
        if (thisGpsPart.stream.size > thatGpsPart.stream.size) thisGpsPart else thatGpsPart,
        if (thisDistPart.stream.size > thatDistPart.stream.size) thisDistPart else thatDistPart,
        // assume we can use attributes from both sources, do not prefer one over another
        mergeAttributes(thisAttrPart, thatAttrPart)
      )
    }

    // distance streams need offsetting
    // when some part missing a distance stream, we need to compute the offset from GPS

    var offset = 0.0
    val offsetStreams = for ((gps, dist, attr) <- streams) yield {
      val partDist = dist.stream.lastOption.fold(gps.distStream.lastOption.fold(0.0)(_._2))(_._2)
      val startOffset = offset
      offset += partDist
      (gps.stream, dist.offsetDist(startOffset).stream, attr)
    }

    val totals = offsetStreams.fold(offsetStreams.head) { case ((totGps, totDist, totAttr), (iGps, iDist, iAttr)) =>
      (totGps ++ iGps, totDist ++ iDist, mergeAttributes(totAttr, iAttr))
    }
    val mergedId = ActivityId(FileId.TempId(id.id.filename), "", id.name, begTime, endTime, sportName, dist.stream.last._2)

    ActivityEvents(mergedId, eventsAndSportsSorted, dist.pickData(totals._2), gps.pickData(totals._1), totals._3).unifySamples
  }

  def split(splitTime: ZonedDateTime): Option[ActivityEvents] = {
    val logging = false

    if (logging) println(s"Split ${id.startTime}..${id.endTime} at $splitTime")

    // we always want to keep the splitTime even if it is not a split event. This happens when deleting part of activities
    // because some split times are suppressed during the process
    val splitEvents = events.filter(e => e.isSplit || e.stamp == splitTime).toSeq

    val splitTimes = splitEvents.map(e => e.stamp)

    assert(splitTimes.contains(id.startTime))
    assert(splitTimes.contains(id.endTime))

    val splitRanges = splitEvents zip splitTimes.tail

    val toSplit = splitRanges.find(_._1.stamp == splitTime)

    toSplit.map { case (beg, endTime) =>

      val begTime = beg.stamp
      if (logging) println(s"keep $begTime..$endTime")

      val eventsRange = events.dropWhile(_.stamp <= begTime).takeWhile(_.stamp < endTime)

      val distRange = dist.pickData(dist.slice(begTime, endTime).stream)
      val gpsRange = gps.pickData(gps.slice(begTime, endTime).stream)

      val attrRange = attributes.map { attr =>
        attr.slice(begTime, endTime)
      }

      val sport = beg.sportChange.getOrElse(id.sportName)

      val act = ActivityEvents(id.copy(startTime = begTime, endTime = endTime, sportName = sport), eventsRange, distRange, gpsRange, attrRange)

      act
    }
  }

  def span(time: ZonedDateTime): (Option[ActivityEvents], Option[ActivityEvents]) = {

    val (takeDist, leftDist) = dist.span(time)
    val (takeGps, leftGps) = gps.span(time)
    val splitAttributes = attributes.map(_.span(time))

    val takeAttributes = splitAttributes.map(_._1)
    val leftAttributes = splitAttributes.map(_._2)

    val (takeEvents, leftEvents) = events.span(_.stamp < time)

    val (takeBegTime, takeEndTime) = (startTime, time)

    val (leftBegTime, leftEndTime) = (time, endTime)

    val takeMove = if (takeBegTime < takeEndTime) {
      Some(ActivityEvents(id.copy(startTime = takeBegTime, endTime = takeEndTime), takeEvents, takeDist, takeGps, takeAttributes))
    } else None
    val leftMove = if (leftBegTime < leftEndTime) {
      Some(ActivityEvents(id.copy(startTime = leftBegTime, endTime = leftEndTime), leftEvents, leftDist, leftGps, leftAttributes))
    } else None

    (takeMove, leftMove)
  }

  def timeOffset(offset: Int): ActivityEvents = {
    copy(
      id = id.timeOffset(offset),
      events = events.map(_.timeOffset(offset)),
      gps = gps.timeOffset(offset),
      dist = dist.timeOffset(offset),
      attributes = attributes.map(_.timeOffset(offset)))
  }

  def processPausesAndEvents: ActivityEvents = {
    val timing = Timing.start()
    //val cleanLaps = laps.filter(l => l > actId.startTime && l < actId.endTime)

    // prefer GPS, as this is already cleaned for accuracy error
    val distStream = if (this.gps.isEmpty) {
      DataStreamGPS.distStreamFromRouteStream(this.dist.stream.toSeq)
    } else {
      this.gps.distStream
    }

    timing.logTime("distStream")

    val speedStream = DataStreamGPS.computeSpeedStream(distStream)
    val speedMap = speedStream

    // integrate route distance back from smoothed speed stream so that we are processing consistent data
    val routeDistance = DataStreamGPS.routeStreamFromSpeedStream(speedStream)

    timing.logTime("routeDistance")

    // find pause candidates: times when smoothed speed is very low
    val speedPauseMax = 0.7
    val speedPauseAvg = 0.4
    val minPause = 10 // minimal pause to record
    val minLongPause = 20 // minimal pause to introduce end pause event
    val minSportChangePause = 50  // minimal pause to introduce automatic transition between sports
    val minSportDuration = 15 * 60 // do not change sport too often, assume at least 15 minutes of activity

    // select samples which are slow and the following is also slow (can be in the middle of the pause)
    type PauseStream = List[(ZonedDateTime, ZonedDateTime, Double)]
    val pauseSpeeds: PauseStream = (speedStream zip speedStream.drop(1)).collect {
      case ((t1, _), (t2, s)) if s < speedPauseMax => (t1, t2, s)
    }.toList
    // aggregate pause intervals - merge all
    def mergePauses(pauses: PauseStream, done: PauseStream): PauseStream = {
      pauses match {
        case head :: next :: tail =>
          if (head._2 == next._1) { // extend head with next and repeat
            mergePauses(head.copy(_2 = next._2) :: tail, done)
          } else { // head can no longer be extended, use it, continue processing
            mergePauses(next +: tail, head +: done)
          }
        case _ => pauses ++ done
      }
    }

    val mergedPauses = mergePauses(pauseSpeeds, Nil).reverse

    timing.logTime("mergePauses")

    def avgSpeedDuring(beg: ZonedDateTime, end: ZonedDateTime): Double = {
      val findBeg = routeDistance.to(beg).lastOption
      val findEnd = routeDistance.from(end).headOption
      val avgSpeed = for (b <- findBeg; e <- findEnd) yield {
        val duration = ChronoUnit.SECONDS.between(b._1, e._1)
        if (duration > 0) (e._2 - b._2) / duration else 0
      }
      avgSpeed.getOrElse(0)
    }

    type Pause = (ZonedDateTime, ZonedDateTime)
    def pauseDuration(p: Pause) = timeDifference(p._1, p._2)

    // take a pause candidate and reduce its size until we get a real pause (or nothing)
    def extractPause(beg: ZonedDateTime, end: ZonedDateTime): List[Pause] = {

      val pauseArea = speedStream.from(beg).to(end)

      // locate a point which is under required avg speed, this is guaranteed to serve as a possible pause center
      val (_, candidateStart) = pauseArea.span(_._2 > speedPauseAvg)
      val (candidate, _) = candidateStart.span(_._2 <= speedPauseAvg)
      // now take all under the speed

      def isPauseDuring(b: ZonedDateTime, e: ZonedDateTime, rect: DataStreamGPS.GPSRect) = {
        val gpsRange = gps.stream.from(b).to(e)

        val extendRect = for {
          gpsBeg <- gpsRange.headOption
          gpsEnd <- gpsRange.lastOption
        } yield {
          rect.merge(gpsBeg._2).merge(gpsEnd._2)
        }
        val extendedRect = extendRect.getOrElse(rect)
        val rectSize = extendedRect.size
        val rectDuration = ChronoUnit.SECONDS.between(b, e)
        val rectSpeed = if (rectDuration > 0) rectSize / rectDuration else 0
        // until the pause is long enough, do not evaluate its speed
        (rectSpeed < speedPauseAvg || rectDuration < minPause, extendedRect)
      }

      def extendPause(b: ZonedDateTime, e: ZonedDateTime, rect: DataStreamGPS.GPSRect): Pause = {
        // try extending beg first
        // b .. e is inclusive
        val prevB = pauseArea.to(b).dropRight(1).lastOption.map(_._1)
        val nextE = pauseArea.from(e).drop(1).headOption.map(_._1)

        val pauseB = prevB.map(isPauseDuring(_, e, rect))
        val pauseE = nextE.map(isPauseDuring(b, _, rect))
        if (pauseB.isDefined && pauseB.get._1) {
          extendPause(prevB.get, e, pauseB.get._2)
        } else if (pauseE.isDefined && pauseE.get._1) {
          extendPause(b, nextE.get, pauseE.get._2)
        } else {
          (beg, end)
        }
      }

      val candidateRange = for {
        b <- candidate.headOption
        e <- candidate.lastOption
      } yield {
        (b._1, e._1)
      }

      val candidatePause = candidateRange.toList.flatMap { case (cb, ce) =>
        val gpsRange = gps.stream.from(cb).to(ce)
        val gpsRect = gpsRange.foldLeft(new DataStreamGPS.GPSRect(gpsRange.head._2))((rect, p) => rect merge p._2)
        val cp = extendPause(cb, ce, gpsRect)
        // skip the extended pause
        val next = pauseArea.from(cp._2).drop(1).headOption
        next.map(n => cp :: extractPause(n._1, end)).getOrElse(List(cp))
      }
      candidatePause
    }

    def cleanPauses(ps: List[Pause]): List[Pause] = {
      // when pauses are too close to each other, delete them or merge them
      def recurse(todo: List[Pause], done: List[Pause]): List[Pause] = {
        def shouldBeMerged(first: (ZonedDateTime, ZonedDateTime), second: (ZonedDateTime, ZonedDateTime)) = {
          timeDifference(first._2, second._1) < 120 && avgSpeedDuring(first._2, second._1) < 2
        }

        def shouldBeDiscardedFirst(first: (ZonedDateTime, ZonedDateTime), second: (ZonedDateTime, ZonedDateTime)) = {
          timeDifference(first._2, second._1) < 240
        }

        todo match {
          case first :: second :: tail if shouldBeMerged(first, second) =>
            recurse((first._1, second._2) :: tail, done)
          case first :: second :: tail if shouldBeDiscardedFirst(first, second) =>
            val longer = Seq(first, second).maxBy(pauseDuration)
            recurse(longer :: tail, done)
          case head :: tail =>
            recurse(tail, head :: done)
          case _ =>
            done
        }
      }
      var cleaned = recurse(ps, Nil).reverse

      // if there are too many pauses, remove the shortest ones
      breakable {
        while (cleaned.nonEmpty) {
          // find shortest pause
          // 10 pauses: keep only pauses above 100 seconds
          val limit = cleaned.size * 10

          val minPause = cleaned.minBy(pauseDuration)

          if (pauseDuration(minPause) > limit) break()

          cleaned = cleaned.patch(cleaned.indexOf(minPause), Nil, 1)
        }
      }
      cleaned
    }

    val extractedPauses = mergedPauses.flatMap(p => extractPause(p._1, p._2))

    timing.logTime("extractedPauses")

    val cleanedPauses = cleanPauses(extractedPauses)

    val pauseEvents = cleanedPauses.flatMap { case (tBeg, tEnd) =>
      val duration = ChronoUnit.SECONDS.between(tBeg, tEnd).toInt
      if (duration > minLongPause) {
        Seq(PauseEvent(duration, tBeg), PauseEndEvent(duration, tEnd))
      } else if (duration > minPause) {
        Seq(PauseEvent(duration, tBeg))
      } else Seq()
    }

    def collectSportChanges(todo: List[Pause], done: List[Pause]): List[Pause] = {
      todo match {
        case first :: second :: tail if timeDifference(first._2, second._1) < minSportDuration =>
          val longer = Seq(first, second).maxBy(pauseDuration)
          collectSportChanges(longer :: tail, done)
        case head :: tail if timeDifference(head._1, head._2) > minSportChangePause =>
          collectSportChanges(tail, head :: done)
        case _ :: tail =>
          collectSportChanges(tail, done)
        case _ =>
          done
      }
    }

    val sportChangePauses = collectSportChanges(cleanedPauses, Nil).reverse

    val sportChangeTimes = sportChangePauses.flatMap(p => Seq(p._1, p._2))

    val intervalTimes = (id.startTime +: sportChangeTimes :+ id.endTime).distinct

    def speedDuringInterval(beg: ZonedDateTime, end: ZonedDateTime) = {
      speedMap.from(beg).to(end)
    }

    def intervalTooShort(beg: ZonedDateTime, end: ZonedDateTime) = {
      val duration = ChronoUnit.SECONDS.between(beg, end)
      val distance = avgSpeedDuring(beg, end) * duration
      duration < 60 && distance < 100
    }

    val intervals = intervalTimes zip intervalTimes.drop(1)

    val sportsInRanges = intervals.flatMap { case (pBeg, pEnd) =>

      assert(pEnd > pBeg)
      if (sportChangePauses.exists(_._1 == pBeg) || intervalTooShort(pBeg, pEnd)) {
        None // no sport detection during pauses (would always detect as something slow, like Run
      } else {

        val spd = speedDuringInterval(pBeg, pEnd)

        val speedStats = DataStreamGPS.speedStats(spd)

        val sport = detectSportBySpeed(speedStats, id.sportName)

        Some(pBeg, sport)
      }
    }

    // reversed, as we will be searching for last lower than
    val sportsByTime = sportsInRanges.sortBy(_._1)(Ordering[ZonedDateTime].reverse)

    def findSport(time: ZonedDateTime) = {
      sportsByTime.find(_._1 <= time).map(_._2).getOrElse(id.sportName)
    }

    // process existing events
    val inheritEvents = this.events.filterNot(_.isSplit)

    val hillEvents = findHills(gps, distStream)

    val events = (BegEvent(id.startTime, findSport(id.startTime)) +: EndEvent(id.endTime) +: inheritEvents) ++ pauseEvents ++ hillEvents
    val eventsByTime = events.sortBy(_.stamp)

    val sports = eventsByTime.map(x => findSport(x.stamp))

    // insert / modify splits on edges
    val sportChange = (("" +: sports) zip sports).map(ab => ab._1 != ab._2)
    val allEvents = (eventsByTime, sports, sportChange).zipped.map { case (ev, sport,change) =>
      // TODO: handle multiple events at the same time
      if (change) {
        if (ev.isInstanceOf[BegEvent]) BegEvent(ev.stamp, sport)
        else SplitEvent(ev.stamp, sport)
      }
      else ev
    }

    // when there are multiple events at the same time, use only the most important one
    @tailrec
    def cleanupEvents(es: List[Event], ret: List[Event]): List[Event] = {
      es match {
        case first :: second :: tail if first.stamp == second.stamp =>
          if (first.order < second.order) cleanupEvents(first :: tail, ret)
          else cleanupEvents(second :: tail, ret)
        case head :: tail =>
          cleanupEvents(tail, head :: ret)
        case _ =>
          ret
      }
    }

    val cleanedEvents = cleanupEvents(allEvents.sortBy(_.stamp).toList, Nil).reverse

    timing.logTime("extractPause done")

    copy(events = cleanedEvents.toArray)
  }


  /*
  Clean errors in buildings and other areas where signal is bad and position error high
  (EHPE - estimated horizontal positition error)
  * */
  def cleanGPSPositionErrors: ActivityEvents = {

    def vecFromGPS(g: GPSPoint) = Vector2(g.latitude, g.longitude)
    //def gpsFromVec(v: Vector2) = GPSPoint(latitude = v.x, longitude = v.y, None)(None)

    @tailrec
    def cleanGPS(todoGPS: List[gps.ItemWithTime], done: List[gps.ItemWithTime]): List[gps.ItemWithTime] = {
      todoGPS match {
        case first :: second :: tail if second._2.accuracy > 8 =>
          // move second as little as possible to stay within GPS accuracy error
          val gps1 = first._2
          val gps2 = second._2
          val v1 = vecFromGPS(gps1)
          val v2 = vecFromGPS(gps2)

          val maxDist = second._2.accuracy * 2 // * 2 is empirical, tested activity looks good with this value
          val dist = gps1 distance gps2
          // move as far from v2 (as close to v1) as accuracy allows
          if (dist > maxDist) {
            val clamped = (v1 - v2) * (maxDist / dist) + v2
            val gpsClamped = gps1.copy(clamped.x, clamped.y)(None)
            cleanGPS(second.copy(_2 = gpsClamped) :: tail, first :: done)
          } else {
            cleanGPS(second.copy(_2 = first._2) :: tail, first :: done)
          }
        case head :: tail =>
          cleanGPS(tail, head :: done)
        case _ =>
          done
      }
    }

    val gpsClean = cleanGPS(gps.stream.toList, Nil).reverse
    val gpsStream = gps.pickData(SortedMap(gpsClean:_*))

    // rebuild dist stream as well

    // TODO: DRY
    val distanceDeltas = gpsStream.distStream
    val distances = DataStreamGPS.routeStreamFromDistStream(distanceDeltas.toSeq)

    copy(gps = gpsStream, dist = dist.pickData(distances))

  }

  def cleanPositionErrors: ActivityEvents = {
    if (hasGPS) cleanGPSPositionErrors
    else this
  }

  def unifySamples: ActivityEvents = {
    // make sure all distance and attribute times are aligned with GPS times
    val times = gps.stream.keys.toList
    //dist
    val unifiedAttributes = attributes.map(a => a.samplesAt(times))
    copy(attributes = unifiedAttributes)
  }

  trait Stats {
    def distanceInM: Double
    def totalTimeInSeconds: Double
    def speed: Double
    def movingTime: Double
    def elevation: Double
  }
  def stats: Stats = new Stats {
    val distanceInM = id.distance
    val totalTimeInSeconds = duration
    val speed = distanceInM / totalTimeInSeconds
    val movingTime = 0.0
    val elevation = self.elevation
  }
}


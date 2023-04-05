package com.github.opengrabeso.mixtio

import java.time.temporal.ChronoUnit

import java.time.{ZonedDateTime, Duration => JDuration}

import scala.collection.immutable.SortedMap
import common.Util._
import common.model._
import shared.Timing

import scala.annotation.tailrec

@SerialVersionUID(12L)
case class GPSPoint(latitude: Double, longitude: Double, elevation: Option[Int])(val in_accuracy: Option[Double]) {
  override def toString = {
    s"GPSPoint($latitude,$longitude,$elevation)($in_accuracy)"
  }

  @transient
  def accuracy: Double = if (in_accuracy != null) in_accuracy.getOrElse(0) else 0

  def distance(that: GPSPoint): Double = {
    GPS.distance(this.latitude, this.longitude, that.latitude, that.longitude)
  }
}

case class HRPoint(hr: Int, dist: Double)

object DataStream {
  def distanceIsAlmostEmpty(begDist: Double, endDist: Double, begTime: ZonedDateTime, endTime: ZonedDateTime): Boolean = {
    val dist = endDist - begDist
    val duration = timeDifference(begTime, endTime)
    val maxSpeed = 0.1
    dist < duration * maxSpeed

  }

  def mapStreamValues[Item, T](stream: SortedMap[ZonedDateTime, Item], f: Item => T): SortedMap[ZonedDateTime, T] = {
    // note: not very fast, rebuilds the SortedMap
    // mapValues however creates a copy which is not serializable
    stream.transform((_, v) => f(v))
  }

  // remove any points which contain no useful information
  def optimize[Data <: DataStream](data: Data): Data = {
    type ItemWithTime = data.ItemWithTime

    def notNeeded(first: ItemWithTime, second: ItemWithTime) = {
      // avoid samples being too apart each other, it brings no benefit and could be risky
      if (timeDifference(first._1, second._1) > 30) {
        false
      } else {
        first._2 == second._2
      } // Strava seems not to be interpolating HR, it simply reuses the last value seen
    }

    @tailrec
    def optimizeRecurse(todo: List[ItemWithTime], done: List[ItemWithTime]): List[ItemWithTime] = {
      todo match {
        case first :: second :: tail if notNeeded(first, second) =>
          optimizeRecurse(first :: tail, done)
        case head :: tail =>
          optimizeRecurse(tail, head :: done)
        case Nil => done
      }
    }

    val newStream = SortedMap(optimizeRecurse(data.stream.toList, Nil):_*)
    data.pickData(newStream).asInstanceOf[Data]
  }

  // provide samples at given timestamps
  def samplesAt[Data <: DataStream](data: Data, times: List[ZonedDateTime]): Data = {
    type ItemWithTime = data.ItemWithTime

    // select nearest sample

    @tailrec
    def samplesAtRecurse(todoTimes: List[ZonedDateTime], todo: List[ItemWithTime], done: List[ItemWithTime]): List[ItemWithTime] = {
      todoTimes match {
        case head :: tail =>
          // interpolate or get nearest
          val (toTime, fromTime) = todo.span(_._1 <= head)

          val lastToTime = if (todo.nonEmpty) toTime.lastOption.orElse(done.headOption) else None

          val adjustTime = lastToTime.map(sample => sample.copy(_1 = head))

          samplesAtRecurse(tail, fromTime, adjustTime.toList ++ done)
        case Nil => done
      }
    }

    val newStream = SortedMap(samplesAtRecurse(times, data.stream.toList, Nil):_*)
    data.pickData(newStream).asInstanceOf[Data]
  }

  type EventTimes = List[ZonedDateTime]

  type GPSPointWithTime = (ZonedDateTime, GPSPoint)

  type DistStream  = SortedMap[ZonedDateTime, Double]
  type DistList  = List[(ZonedDateTime, Double)]

  object FilterSettings {
    def none = new FilterSettings(0, 0, "None")
    def weak = new FilterSettings(5, 3, "Weak")
    def normal = new FilterSettings(9, 5, "Normal")
    def strong = new FilterSettings(13, 7, "Strong")
    val list = IndexedSeq(none, weak, normal, strong)
    def names = list.map(_.name).zipWithIndex

    def select(i: Int): FilterSettings = if (i<0) list(0) else if (i>=list.size) list.last else list(i)
  }
  class FilterSettings(val slidingWindow: Int, val useMiddle: Int, val name: String)

}

import DataStream._

@SerialVersionUID(10L)
sealed abstract class DataStream extends Serializable {


  def typeToLog: String
  def streamType: Class[_ <: DataStream] = this.getClass

  type Item

  type ItemWithTime = (ZonedDateTime, Item)

  type DataMap = SortedMap[ZonedDateTime, Item]

  def stream: DataMap

  def pickData(data: DataMap): DataStream

  def mapStreamValues[T](f: Item => T): SortedMap[ZonedDateTime, T] = DataStream.mapStreamValues(stream, f)

  val startTime: Option[ZonedDateTime] = stream.headOption.map(_._1)
  val endTime: Option[ZonedDateTime] = stream.lastOption.map(_._1)

  def inTimeRange(b: ZonedDateTime, e: ZonedDateTime): Boolean = {
    startTime.forall(_ >= b) && endTime.forall(_ <= e)
  }

  // should be discarded
  def isEmpty: Boolean = stream.isEmpty
  def nonEmpty: Boolean = stream.nonEmpty
  def isAlmostEmpty: Boolean

  // must not be discarded
  def isNeeded: Boolean

  def span(time: ZonedDateTime): (DataStream, DataStream) = {
    val (take, left) = stream.span(_._1 < time)
    (pickData(take), pickData(left))
  }

  def slice(timeBeg: ZonedDateTime, timeEnd: ZonedDateTime): DataStream = {
    val subData = stream.from(timeBeg).to(timeEnd)
    pickData(subData)
  }

  def sliceKeepPrevious(timeBeg: ZonedDateTime, timeEnd: ZonedDateTime): DataStream = {
    // include last previous value of the attribute if available
    // as Strava works this way: until a new attribute value is provided, the previous one is assumed to be valid (verified for HR and temperature)
    val subData = stream.from(timeBeg).to(timeEnd)
    val prevData = stream.to(timeBeg)
    val withLeadValue = if (subData.contains(timeBeg) || prevData.isEmpty) {
      subData
    } else {
      subData + (timeBeg -> prevData.head._2)
    }
    pickData(withLeadValue)
  }

  def timeOffset(bestOffset: Int): DataStream = {
    val adjusted = stream.map{
      case (k,v) =>
        k.plus(bestOffset, ChronoUnit.SECONDS) -> v
    }
    pickData(adjusted)
  }

  def samplesAt(times: List[ZonedDateTime]): DataStream = DataStream.samplesAt(this, times)

  def optimize(eventTimes: EventTimes): DataStream

  def toLog = s"$typeToLog: ${startTime.map(_.toLog).getOrElse("")} .. ${endTime.map(_.toLogShort).getOrElse("")}"

  override def toString = toLog

}

object DataStreamGPS {
  case class GPSRect(latMin: Double, latMax: Double, lonMin: Double, lonMax: Double) {
    def this(item: GPSPoint) = {
      this(item.latitude, item.latitude, item.longitude, item.longitude)
    }

    def merge(that: GPSPoint) = {
      copy(
        latMin = that.latitude min latMin, latMax = that.latitude max latMax,
        lonMin = that.longitude min lonMin, lonMax = that.longitude max lonMax
      )
    }

    // diagonal size of the rectangle
    def size: Double = GPS.distance(latMin, lonMin, latMax, lonMax)
  }

  def rectAlmostEmpty(rect: GPSRect, timeBeg: ZonedDateTime, timeEnd: ZonedDateTime): Boolean = {
    val d = rect.size
    val duration = timeDifference(timeBeg, timeEnd).abs
    val maxSpeed = 0.2
    d <= (maxSpeed * duration min 100)
  }

  /**
    * Experiments have shown smoothingInterval = 60 gives most accurate results.
    * Perhaps the same smoothing interval is used in the Quest itself?
    */
  private val smoothingInterval = 60

  private def smoothSpeed(input: DistStream, durationSec: Double): DistStream = {

    import Function._

    // fixSpeed(input) was called here, but it was used only because of sample timestamp misunderstanding
    val smoothedList = smoothing(input.toList, durationSec)
    SortedMap(smoothedList:_*)
  }

  def distStreamFromGPSList(gps: Seq[GPSPoint]): Seq[Double] = {
    val gpsDistances = (gps zip gps.drop(1)).map {case (a,b) => a distance b}
    gpsDistances
  }

  // differential distance stream from GPS data
  def distStreamFromGPS(gps: SortedMap[ZonedDateTime, GPSPoint]): DistStream = {
    if (gps.nonEmpty) {
      val gpsKeys = gps.keys.toSeq // toSeq needed to preserve order
      val gpsValues = gps.values.toSeq
      val gpsPairs = gpsKeys.drop(1) zip (gpsValues zip gpsValues.drop(1))
      val gpsDistances = gpsPairs.map { case (t, (pa, pb)) => t -> (pa distance pb) }
      SortedMap((gpsKeys.head -> 0.0) +: gpsDistances: _*)
    } else {
      SortedMap()
    }
  }

  // total (along route) distance from GPS data
  def routeStreamFromGPS(gps: SortedMap[ZonedDateTime, GPSPoint]): DistStream = {
    val deltas = distStreamFromGPS(gps)
    routeStreamFromDistStream(deltas.toSeq)
  }

  def routeStreamFromDistStream(distDeltas: Seq[(ZonedDateTime, Double)]): DistStream = {
    val route = distDeltas.scanLeft(0d) { case (sum, (_, d)) => sum + d }
    // scanLeft adds initial value as a first element - use tail to drop it
    val ret = distDeltas.map(_._1) zip route.tail
    SortedMap.from(ret)
  }

  def distStreamFromRouteStream(dist: Seq[(ZonedDateTime, Double)]): DistStream = {
    val times = dist.map(_._1)
    val routeValues = dist.map(_._2)
    val distValues = 0.0 +: (routeValues zip routeValues.drop(1)).map(p => p._2 - p._1)
    val ret = times zip distValues
    SortedMap.from(ret)
  }

  def routeStreamFromSpeedStream(distDeltas: DistStream): DistStream = {
    if (distDeltas.isEmpty) SortedMap()
    else {
      assert(distDeltas.head._2 == 0)
      val route = distDeltas.tail.scanLeft(distDeltas.head) { case ((tSum, dSum), (t, d)) =>
        val dt = ChronoUnit.SECONDS.between(tSum, t)
        t -> (dSum + d * dt)
      }
      SortedMap.from(route)
    }
  }

  def computeSpeedStream(dist: DistStream, smoothing: Int = 10): DistStream = {
    val timing = Timing.start(true)
    val smoothedSpeed = smoothSpeed(dist, smoothing)
    timing.logTime(s"computeSpeedStream of ${dist.size} samples, smoothing $smoothing")
    smoothedSpeed
  }

  /**
    * @return median, 80% percentile, max
    * */
  def speedStats(speedStream: DistStream): SpeedStats = {
    //implicit val start = Timing.Start()

    if (speedStream.isEmpty) SpeedStats(0,0,0)
    else {

      val toKmh = 3.6
      val speeds = speedStream.map(_._2 * toKmh)

      val max = speeds.max
      val min = speeds.min

      val num_bins = 40

      val histogram = speeds
        .map(x => (((x - min) / (max - min)) * num_bins).floor.toInt)
        .groupBy(identity)
        .map(x => x._1 -> x._2.size)
        .toSeq
        .sortBy(_._1)

      def percentile(percent: Double) = {
        val countUnder = (percent * 0.01 * speeds.size).toInt

        def percentileRecurse(countLeft: Int, histLeft: Seq[(Int, Int)]): Int = {
          if (histLeft.isEmpty) num_bins
          else if (histLeft.head._2 >= countLeft) histLeft.head._1
          else percentileRecurse(countLeft - histLeft.head._2, histLeft.tail)
        }

        val slot = percentileRecurse(countUnder, histogram)
        slot.toDouble / num_bins * (max - min) + min
      }


      val med = percentile(50)

      val fast = percentile(80)

      val top = percentile(99)

      //Timing.logTime(s"Speed of ${speedStream.size} samples")
      SpeedStats(med, fast, top)
    }
  }

  type ValueList = List[GPSPointWithTime]
  @tailrec
  private def detectEmptyPrefix(begTime: ZonedDateTime, rect: GPSRect, stream: ValueList, ret: Option[(ZonedDateTime, GPSRect)]): Option[(ZonedDateTime, GPSRect)] = {
    stream match {
      case Nil => ret
      case head :: tail =>
        val newRect = rect merge head._2
        val newRet = if (rectAlmostEmpty(rect, begTime, head._1)) Some((head._1, newRect)) else ret
        detectEmptyPrefix(begTime, newRect, tail, newRet)
    }
  }

  def dropEmptyPrefix(stream: ValueList, timeOffset: JDuration, compare: (ZonedDateTime, ZonedDateTime) => Boolean): ZonedDateTime = {
    val prefixTime = detectEmptyPrefix(stream.head._1, new GPSRect(stream.head._2), stream, None)
    prefixTime.map { case (prefTime, prefRect) =>
      // trace back the prefix rectangle size
      val backDistance = prefRect.size

      val prefixRaw = stream.takeWhile(t => compare(t._1, prefTime))

      val gpsDist = DataStreamGPS.distStreamFromGPSList(prefixRaw.map(_._2)).reverse

      @scala.annotation.tailrec
      def trackBackDistance(distances: Seq[Double], trace: Double, ret: Int): Int = {
        if (trace <=0 || distances.isEmpty) ret
        else {
          trackBackDistance(distances.tail, trace - distances.head, ret + 1)
        }
      }

      val backDistanceEnd = trackBackDistance(gpsDist, backDistance, 0)
      val prefixValidated = prefixRaw.dropRight(backDistanceEnd)

      val timeValidated = prefixValidated.last._1

      val offsetPrefTime = timeValidated.plus(timeOffset)
      val edgeTime = if (compare(offsetPrefTime, stream.head._1)) stream.head._1 else offsetPrefTime
      edgeTime
    }.getOrElse(stream.head._1)
  }

  // drop beginning and end with no activity
  def dropAlmostEmpty(stream: ValueList): Option[(ZonedDateTime, ZonedDateTime)] = {
    if (stream.nonEmpty) {
      val droppedPrefixTime = dropEmptyPrefix(stream, JDuration.ofSeconds(-10), _ <= _)
      val droppedPostfixTime = dropEmptyPrefix(stream.reverse, JDuration.ofSeconds(+10), _ >= _)
      if (droppedPrefixTime >= droppedPostfixTime) None
      else Some((droppedPrefixTime, droppedPostfixTime))
    } else None
  }


  private def distStreamToCSV(ds: DistStream): String = {
    val times = ds.keys.toSeq
    val diffs = 0L +: (times zip times.drop(1)).map { case (t1, t2) => ChronoUnit.MILLIS.between(t1, t2) }
    (ds zip diffs).map { case (kv, duration) =>
      s"${kv._1},${duration/1000.0},${kv._2}"
    }.mkString("\n")
  }

  type GPSStream  = SortedMap[ZonedDateTime, GPSPoint]
}

@SerialVersionUID(10L)
class DataStreamGPS(override val stream: DataStreamGPS.GPSStream) extends DataStream {

  import DataStreamGPS._

  type Item = GPSPoint


  def typeToLog: String = "GPS"

  override def pickData(data: DataMap) = new DataStreamGPS(data)
  override def slice(timeBeg: ZonedDateTime, timeEnd: ZonedDateTime): DataStreamGPS = super.slice(timeBeg, timeEnd).asInstanceOf[DataStreamGPS]
  override def timeOffset(bestOffset: Int): DataStreamGPS = super.timeOffset(bestOffset).asInstanceOf[DataStreamGPS]


  override def span(time: ZonedDateTime): (DataStreamGPS, DataStreamGPS) = {
    val ret = super.span(time)
    (ret._1.asInstanceOf[DataStreamGPS], ret._2.asInstanceOf[DataStreamGPS])
  }

  override def isAlmostEmpty: Boolean = {
    if (stream.isEmpty) true
    else {
      val lat = stream.values.map(_.latitude)
      val lon = stream.values.map(_.longitude)
      // http://www.movable-type.co.uk/scripts/latlong.html
      val rect = GPSRect(lat.min, lat.max, lon.min, lon.max)

      rectAlmostEmpty(rect, stream.head._1, stream.last._1)
    }
  }

  override def isNeeded = false

  lazy val distStream: DistStream = distStreamFromGPS(stream)


  private def computeSpeedStream: DistStream = {

    val gpsDistances = distStreamFromGPS(stream)

    val smoothedSpeed = smoothSpeed(gpsDistances, smoothingInterval)
    smoothedSpeed
  }

  //noinspection ScalaUnusedSymbol
  private def rawToCSV: String = {
    val dist = distStreamFromGPS(stream)
    distStreamToCSV(dist)
  }

  //noinspection ScalaUnusedSymbol
  private def smoothedToCSV: String = {
    val dist = distStreamFromGPS(stream)
    val smooth = smoothSpeed(dist, smoothingInterval)
    distStreamToCSV(smooth)
  }

  def optimize(eventTimes: EventTimes): DataStreamGPS = {

    case class EventsCursor(eventsBefore: List[ZonedDateTime], eventsAfter: List[ZonedDateTime]) {
      def advance(t: ZonedDateTime): EventsCursor = {
        if (eventsAfter.headOption.exists(_ <= t)) EventsCursor(eventsAfter.head :: eventsBefore, eventsAfter.tail)
        else this
      }

      def timeDistanceToEvent(t: ZonedDateTime): Double = {
        var min = Double.MaxValue
        def check(l: List[ZonedDateTime]): Unit = {
          if (l.nonEmpty) {
            min = math.min(min, timeDifference(l.head, t).abs)
          }
        }
        check(eventsBefore)
        check(eventsAfter)
        min
      }

    }

    //type GPSPointWithTime =
    def secondNotNeeded(first: GPSPointWithTime, second: GPSPointWithTime, third: GPSPointWithTime, currentEvents: EventsCursor) = {
      // check if second can be obtained by interpolating first and third
      val secondFactor = timeDifference(first._1, second._1) / timeDifference(first._1, third._1)

      def lerp(value1: Double, value2: Double, f: Double) = value1 + (value2 - value1) * f

      val interpolatedLat = lerp(first._2.latitude, third._2.latitude, secondFactor)
      val interpolatedLon = lerp(first._2.longitude, third._2.longitude, secondFactor)
      // ignore elevation, it is too chaotic anyway
      val secondInterpolated = GPSPoint(latitude = interpolatedLat, longitude = interpolatedLon, elevation = None)(None)
      val dist = secondInterpolated distance second._2
      val maxDist = 1
      val timeDist = currentEvents.timeDistanceToEvent(second._1)
      val error = dist / maxDist + 3 / (timeDist max 1)
      val maxError = 1
      error < maxError
    }


    // remove unnecessary GPS points
    val minTimeSpacing = 10 // seconds
    @tailrec
    def optimizeGPSRecurse(todo: List[GPSPointWithTime], done: List[GPSPointWithTime], currentEvents: EventsCursor): List[GPSPointWithTime] = {
      todo match {
        case first :: second :: third :: tail if timeDifference(first._1, second._1) < minTimeSpacing && secondNotNeeded(first, second, third, currentEvents) =>
          optimizeGPSRecurse(first :: third :: tail, done, currentEvents.advance(third._1))
        case first :: second :: third :: tail =>
          optimizeGPSRecurse(second :: third :: tail, first :: done, currentEvents.advance(third._1))
        case head :: tail =>
          optimizeGPSRecurse(tail, head :: done, currentEvents)
        case Nil =>
          done
      }
    }

    val optimized = optimizeGPSRecurse(stream.toList, Nil, EventsCursor(Nil, eventTimes))

    pickData(SortedMap(optimized:_*))
  }

}

@SerialVersionUID(10L)
class DataStreamLap(override val stream: SortedMap[ZonedDateTime, String]) extends DataStream {
  type Item = String

  def typeToLog: String = "Laps"

  override def pickData(data: DataMap) = new DataStreamLap(data)
  override def isAlmostEmpty = false
  override def isNeeded = true
  def dropAlmostEmpty: DataStreamLap = this

  override def optimize(eventTimes: EventTimes) = this // TODO: remove duplicity or very close laps
}

@SerialVersionUID(11L)
class DataStreamHR(stream: SortedMap[ZonedDateTime, Int]) extends DataStreamAttrib("HR", stream) {

  override def pickData(data: DataMap) = new DataStreamHR(data)
  override def isAlmostEmpty = false
  override def isNeeded = false
  override def dropAlmostEmpty: DataStreamHR = this // TODO: drop

  override def optimize(eventTimes: EventTimes): DataStreamHR = DataStream.optimize(this)

  def removeAboveMax(maxHR: Int): DataStreamHR = {

    val validatedHR = stream.map { case (key, hr) =>
      if (hr > maxHR) key -> None
      else key -> Some(hr)
    }

    // drop two samples around each None
    // TODO: drop time region instead of a count, using Function.Window

    val validatedCleanedHR = slidingRepeatHeadTail(validatedHR.toVector, 5) {
      case s5 if !s5.exists(_._2.isEmpty) => s5(2)
      case s5 => s5(2)._1 -> None
    }.flatMap { case (k, v) =>
        v.map(k -> _)
    }

    pickData(SortedMap(validatedCleanedHR.toSeq:_*))
  }
}

@SerialVersionUID(11L)
class DataStreamAttrib(val attribName: String, override val stream: SortedMap[ZonedDateTime, Int]) extends DataStream {
  type Item = Int

  def typeToLog = attribName

  override def pickData(data: DataMap) = new DataStreamAttrib(attribName, data)
  override def isAlmostEmpty = false
  override def isNeeded = false
  def dropAlmostEmpty: DataStreamAttrib = this // TODO: drop

  def optimize(eventTimes: EventTimes): DataStreamAttrib = DataStream.optimize(this)

  override def timeOffset(bestOffset: Int): DataStreamAttrib = super.timeOffset(bestOffset).asInstanceOf[DataStreamAttrib]
  override def span(time: ZonedDateTime): (DataStreamAttrib, DataStreamAttrib) = super.span(time).asInstanceOf[(DataStreamAttrib, DataStreamAttrib)]
  override def slice(timeBeg: ZonedDateTime, timeEnd: ZonedDateTime): DataStreamAttrib = {
    super.sliceKeepPrevious(timeBeg, timeEnd).asInstanceOf[DataStreamAttrib]
  }
  override def samplesAt(times: List[ZonedDateTime]): DataStreamAttrib = super.samplesAt(times).asInstanceOf[DataStreamAttrib]

}


@SerialVersionUID(10L)
class DataStreamDist(override val stream: SortedMap[ZonedDateTime, Double]) extends DataStream {

  def typeToLog = "Dist"

  type Item = Double

  def rebase: DataStreamDist = {
    if (stream.isEmpty) this
    else {
      val base = stream.head._2
      new DataStreamDist(mapStreamValues(_ - base))
    }
  }

  def offsetDist(dist: Double): DataStreamDist = new DataStreamDist(mapStreamValues(_ + dist))

  def distanceForTime(time: ZonedDateTime): Double = {
    stream.from(time).headOption.map(_._2).getOrElse(stream.last._2)
  }

  override def isAlmostEmpty = stream.isEmpty || DataStream.distanceIsAlmostEmpty(stream.head._2, stream.last._2, stream.head._1, stream.last._1)
  override def isNeeded = false

  override def pickData(data: DataMap): DataStreamDist = new DataStreamDist(data).rebase
  override def slice(timeBeg: ZonedDateTime, timeEnd: ZonedDateTime): DataStreamDist = super.slice(timeBeg, timeEnd).asInstanceOf[DataStreamDist]
  override def timeOffset(bestOffset: Int): DataStreamDist = super.timeOffset(bestOffset).asInstanceOf[DataStreamDist]

  override def span(time: ZonedDateTime): (DataStreamDist, DataStreamDist) = {
    val ret = super.span(time)
    (ret._1.asInstanceOf[DataStreamDist], ret._2.asInstanceOf[DataStreamDist])
  }


  def dropAlmostEmpty: DataStreamDist = this // TODO: drop

  override def optimize(eventTimes: EventTimes) = this // TODO: distance optimization requires interpolation in distanceForTime
}


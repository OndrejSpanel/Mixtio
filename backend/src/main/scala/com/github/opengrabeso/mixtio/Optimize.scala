package com.github.opengrabeso.mixtio

import DataStream._
import common.Util._
import shared.Timing

import java.time.ZonedDateTime
import DataStream.GPSPointWithTime
import requests.BackgroundTasks
import common.model._
import mapbox._

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import Main.StravaAuthResult

object Optimize {

  private def segmentTitle(kind: String, e: SegmentTitle): String = {
    val segPrefix = if (e.isPrivate) "private segment " else "segment "
    val segmentName = Main.shortNameString(e.name, 32 - segPrefix.length - kind.length)
    val complete = if (e.segmentId != 0) {
      kind + segPrefix + <a title={e.name} href={s"https://www.strava.com/segments/${e.segmentId}"}>{segmentName}</a>
    } else {
      kind + segPrefix + segmentName
    }
    complete.capitalize
  }


  def htmlDescription(event: Event): String = event match {
    case e: PauseEvent =>
      s"Pause ${Events.niceDuration(e.duration)}"
    case e: PauseEndEvent =>
      "Pause end"
    case e: LapEvent =>
      "Lap"
    case e: EndEvent =>
      "End"
    case e: BegEvent =>
      "<b>Start</b>"
    case e: SplitEvent =>
      "Split"
    case e: StartSegEvent =>
      segmentTitle("", e)
    case e: EndSegEvent =>
      segmentTitle("end ", e)
    case e: ElevationEvent =>
      Main.shortNameString("Elevation " + e.elev.toInt + " m")
  }



  implicit class DataStreamGPSOps(d: DataStreamGPS) {

    def filterElevation(filter: Int): DataStreamGPS = {
      import d._
      val timing = Timing.start()
      val cache = new GetElevation.TileCache
      // TODO: handle 50 threads per request limitation gracefully
      implicit val threadFactor = BackgroundTasks.currentRequestThreadFactory
      val elevationFutures = stream.toVector.flatMap {
        case (k, v) =>
          v.elevation.map(elev => (k, elev, cache.possibleRange(v.longitude, v.latitude)))
      }

      val elevationStream = elevationFutures.map {
        case (k, elev, rangeFuture) =>
          val range = Await.result(rangeFuture, Duration.Inf)
          (k, range._1 max elev min range._2)
      }

      if (elevationStream.nonEmpty) {
        timing.logTime("All images read")
      }

      val settings = FilterSettings.select(filter)
      import settings._
      val midIndex = slidingWindow / 2
      val filteredElevationData = slidingRepeatHeadTail(elevationStream, slidingWindow){ s =>
        val mid = s(midIndex)
        val values = s.map(_._2)
        // remove extremes, smooth the rest
        val extremes = values.sorted
        val removeFromEachSide = (slidingWindow - useMiddle) / 2
        val withoutExtremes = extremes.slice(removeFromEachSide, removeFromEachSide + useMiddle)
        val avg = if (withoutExtremes.nonEmpty) withoutExtremes.sum / withoutExtremes.size else 0
        mid._1 -> avg
      }.toSeq
      val filteredElevationStream = filteredElevationData.toMap
      val filteredGpsStream = stream.map { case (k, v) =>
        k -> v.copy(elevation = filteredElevationStream.get(k).map(_.toInt))(v.in_accuracy)
      }
      timing.logTime("filterElevation")
      pickData(filteredGpsStream)
    }

  }
  implicit class DataStreamOps[T <: DataStream](d: T) {

  }


  implicit class ActivityEventsOps(eThis: ActivityEvents) {
    def optimize: ActivityEvents = {
      import eThis._
      import Optimize._
      // first optimize all attributes
      val times = eventTimes
      eThis.copy(gps = gps.optimize(times), dist = dist.optimize(times), attributes = attributes.map(_.optimize(times)))
    }

    def optimizeRouteForMap: Seq[(ZonedDateTime, GPSPoint)] = {
      import eThis._
      val maxPoints = 3000
      if (gps.stream.size < maxPoints) gps.stream.toList
      else {
        // first apply generic GPS optimization
        val data = gps.optimize(eventTimes)

        if (data.stream.size < maxPoints) data.stream.toList
        else {
          val ratio = (data.stream.size / maxPoints.toDouble).ceil.toInt
          val gpsSeq = data.stream.toList

          val groups = gpsSeq.grouped(ratio).toList

          // take each n-th
          val allButLast = groups.dropRight(1).map(_.head)
          // always take the last one
          val lastGroup = groups.last
          val last = if (lastGroup.lengthCompare(1) > 0) lastGroup.take(1) ++ lastGroup.takeRight(1)
          else lastGroup

          allButLast ++ last
        }
      }
    }

    def routeJS: String = {
      import eThis._
      val toSend = optimizeRouteForMap

      toSend.map { case (time, g) =>
        val t = id.secondsInActivity(time)
        val d = distanceForTime(time)
        s"[${g.longitude},${g.latitude},$t,$d]"
      }.mkString("[\n", ",\n", "]\n")
    }

    def routeData: Seq[(Double, Double, Double, Double)] = {
      import eThis._
      val toSend = optimizeRouteForMap
      toSend.map { case (time, g) =>
        val t = id.secondsInActivity(time)
        val d = distanceForTime(time)
        (g.longitude, g.latitude, t.toDouble, d)
      }
    }



    def editableEvents: Array[EditableEvent] = {

      val ees = eThis.events.map { e =>
        val action = e.defaultEvent
        EditableEvent(action, eThis.id.secondsInActivity(e.stamp), eThis.distanceForTime(e.stamp), e.listTypes, e.originalEvent, htmlDescription(e))
      }

      // consolidate mutliple events with the same time so that all of them have the same action
      val merged = ees.groupBy(_.time).map { case (t, es) =>
        object CmpEvent extends Ordering[String] {
          def compare(x: String, y: String): Int = {
            def score(et: String) = {
              if (et == "lap") 1
              else if (et.startsWith("split")) 2
              else if (et == "end") -1
              else 0
            }
            score(x) - score(y)
          }
        }
        (t, es.map(_.action).max(CmpEvent))
      }

      ees.map { e => e.copy(action = merged(e.time))}

    }

    /// input filters - elevation filtering, add temperature info
    def applyFilters(auth: StravaAuthResult): ActivityEvents = {
      import eThis._
      val settings = Settings(auth.userId)
      val useElevFilter = id.id match {
        case _: FileId.StravaId =>
          false
        case _ =>
          true
      }
      val elevFiltered = if (useElevFilter) copy(gps = gps.filterElevation(Settings(auth.userId).elevFilter)) else eThis
      val hrFiltered = elevFiltered.attributes.map {
        case hr: DataStreamHR =>
          hr.removeAboveMax(settings.maxHR)
        case attr =>
          attr
      }
      copy(attributes = hrFiltered)
    }


    /// output filters - swim data cleanup
    def applyUploadFilters(auth: StravaAuthResult): ActivityEvents = {
      eThis.id.sportName match {
        case Event.Sport.Swim if eThis.gps.nonEmpty =>
          swimFilter
        case _ =>
          eThis
      }
    }

    // swim filter - avoid large discrete steps which are often found in swim sparse data
    def swimFilter: ActivityEvents = {
      import eThis._
      @tailrec
      def handleInaccuratePartsRecursive(stream: DataStreamGPS.GPSStream, done: DataStreamGPS.GPSStream): DataStreamGPS.GPSStream = {

        def isAccurate(p: (ZonedDateTime, GPSPoint)) = p._2.in_accuracy.exists(_ < 8)

        val (prefix, temp) = stream.span(isAccurate)
        val (handleInner, rest) = temp.span(x => !isAccurate(x))

        if (handleInner.isEmpty) {
          assert(rest.isEmpty)
          done ++ stream
        } else {
          val start = prefix.lastOption orElse done.lastOption // prefer last accurate point if available
          val end = rest.headOption // prefer first accurate point if available
          val handle = handleInner ++ start ++ end
          val duration = timeDifference(handle.head._1, handle.last._1)

          val gpsDistances = DataStreamGPS.routeStreamFromGPS(handle)
          val totalDist = gpsDistances.last._2

          // build gps position by distance curve
          val gpsByDistance = SortedMap((gpsDistances.values zip handle.values).toSeq: _*)

          // found gps data for given distance
          def gpsWithDistance(d: Double): GPSPoint = {
            val get = for {
              prev <- gpsByDistance.rangeTo(d).lastOption
              next <- gpsByDistance.rangeFrom(d).headOption
            } yield {
              def vecFromGPS(g: GPSPoint) = Vector2(g.latitude, g.longitude)

              def gpsFromVec(v: Vector2) = GPSPoint(latitude = v.x, longitude = v.y, None)(None)

              val f = if (next._1 > prev._1) (d - prev._1) / (next._1 - prev._1) else 0
              val p = vecFromGPS(prev._2)
              val n = vecFromGPS(next._2)
              gpsFromVec((n - p) * f + p)
            }
            get.get
          }

          val gpsSwim = for (time <- 0 to duration.toInt) yield {
            val d = (time * totalDist / duration) min totalDist // avoid rounding errors overflowing end of the range
            val t = handle.firstKey.plusSeconds(time)
            t -> gpsWithDistance(d)
          }
          handleInaccuratePartsRecursive(rest, done ++ prefix ++ gpsSwim)
        }

      }

      val gpsSwim = handleInaccuratePartsRecursive(gps.stream, SortedMap.empty)

      copy(gps = gps.pickData(gpsSwim))
    }


  }

}

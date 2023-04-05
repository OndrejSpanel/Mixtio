package com.github.opengrabeso.mixtio
package weather

import java.time.temporal.ChronoUnit
import java.time.ZonedDateTime

import common.Util._

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.util.Try

object GetTemperature {

  def apply(lon: Double, lat: Double, time: ZonedDateTime): Option[Double] = {
    // https://darksky.net/dev/docs
    val secret = Main.secret.darkSkySecret
    val timePar = time.toString().replace(".000", "")

    // consider round all parameters to increase chance of caching
    // longitude / latitude on equator: one degree is about 110 km
    // it may be only less with higher latitudes (85 km on 40deg) - see http://www.longitudestore.com/how-big-is-one-gps-degree.html

    val requestUrl = s"https://api.darksky.net/forecast/$secret/$lat,$lon,$timePar?units=si&exclude=hourly,daily,minutely,flags"

    val request = RequestUtils.buildGetRequest(requestUrl)

    val result = Try {
      val response = request.execute() // TODO: async ?

      val responseJson = RequestUtils.jsonMapper.readTree(response.getContent)

      val tempJson = responseJson.path("currently").path("temperature")

      // TODO: cache darksky.net responses
      tempJson.asDouble
    }

    result.failed.foreach { r =>
      print(s"Weather failure ${r.getLocalizedMessage}")
    }
    result.toOption
  }


  def pickPositions(data: DataStreamGPS, distanceBetweenPoints: Double = 10000, timeBetweenPoints: Double = 3600, altBetweenPoints: Double = 150): DataStreamGPS = {
    // scan distance, each time going over
    @tailrec
    def pickPositionsRecurse(lastPoint: Option[(ZonedDateTime,GPSPoint)], todo: List[(ZonedDateTime, GPSPoint)], done: List[ZonedDateTime]): List[ZonedDateTime] = {
      todo match {
        case head :: tail =>
          if (tail.isEmpty || lastPoint.forall { case (time, pos) =>
            val dist = pos.distance(head._2)
            val elevDiff = pos.elevation.flatMap(pe => head._2.elevation.map(he => (he-pe).abs)).getOrElse(0)
            val duration = ChronoUnit.SECONDS.between(time, head._1)
            val score = (dist / distanceBetweenPoints) + (elevDiff / altBetweenPoints) + (duration / timeBetweenPoints)
            score >= 2
          }) {
            pickPositionsRecurse(Some(head), tail, head._1 :: done)
          } else {
            pickPositionsRecurse(lastPoint, tail, done)
          }
        case _ =>
          done
      }
    }
    val times = pickPositionsRecurse(None, data.stream.toList, Nil).toSet
    val positions = data.stream.view.filterKeys(times.apply).to(SortedMap)
    data.pickData(positions)
  }

  def forPositions(temperaturePos: DataStreamGPS): DataStreamAttrib = {
    val stream = temperaturePos.stream.toSeq.flatMap { case (k, v) =>
      val result = apply(v.longitude, v.latitude, k).map(_.round.toInt)
      result.map(k -> _)
    }
    new DataStreamAttrib("temp", SortedMap(stream:_*))
  }
}

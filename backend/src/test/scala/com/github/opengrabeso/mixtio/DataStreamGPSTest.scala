package com.github.opengrabeso.mixtio

import java.time.ZonedDateTime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DataStreamGPSTest extends AnyFlatSpec with Matchers with SuuntoData {
  behavior of "DataStreamGPS"


  it should "Handle missing samples correctly" in {
    val move = gpsPodMove
    for (m <- move) {
      val gps = m.gps
      val dist = m.dist

      val t = gps.startTime.get
      // sample 271 missing in the GPS stream
      val time = 270 - 1 // <Time>270</Time>

      def relTime(t: ZonedDateTime, r: Int) = t plusSeconds r

      // verify the test data demonstrate the problem
      gps.stream.get(relTime(t, time + 0)) shouldNot be(None)
      gps.stream.get(relTime(t, time + 1)) should be(None)
      gps.stream.get(relTime(t, time + 2)) shouldNot be(None)

      // verify even the dist stream is still missing the data
      val distStream = gps.distStream
      distStream.get(relTime(t, time + 0)) shouldNot be(None)
      distStream.get(relTime(t, time + 1)) should be(None)
      distStream.get(relTime(t, time + 2)) shouldNot be(None)

    }

  }
}

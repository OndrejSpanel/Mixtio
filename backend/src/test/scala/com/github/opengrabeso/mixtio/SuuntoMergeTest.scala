package com.github.opengrabeso.mixtio

import java.time.ZonedDateTime

import org.scalatest.{FlatSpec, Matchers}

class SuuntoMergeTest extends FlatSpec with Matchers with SuuntoData {
  behavior of "SuuntoMerge"

  it should "load Quest file" in {
    val move = questMove

    move.isEmpty shouldBe false

    move.foreach { m =>
      val hr = m.streamGet[DataStreamHR]
      hr.isEmpty shouldBe false

      m.streamGet[DataStreamLap].isEmpty shouldBe false

      val t = ZonedDateTime.parse("2016-10-21T06:46:57Z")
      m.startTime.contains(t)
      m.duration shouldBe 842.4
    }
  }

  it should "load GPS pod file" in {
    val move = gpsPodMove

    move.isEmpty shouldBe false

    move.foreach { m =>
      val gps = m.gps
      gps.stream.isEmpty shouldBe false

      val t = ZonedDateTime.parse("2016-10-21T06:46:01Z")
      m.startTime.compareTo(t) should equal (0)
      m.duration shouldBe 4664.6 +- 0.5
    }

  }

  ignore should "merge GPS + Quest files" in { // failing on GitHub, as quest.xml is always in local timezone
    for (hr <- questMove; gps <- gpsPodMove) {
      val hrActivity = MoveslinkImport.loadFromMove("quest.xml", "", hr)
      assert(!gps.hasAttributes)
      assert(gps.hasGPS)
      assert(hrActivity.get.hasAttributes)
      val m = gps.merge(hrActivity.get)
      assert(m.hasGPS)
      assert(m.hasAttributes)
      m.duration shouldBe 4664.6 +- 0.5
      assert(!m.isAlmostEmpty(30))

    }
  }

}



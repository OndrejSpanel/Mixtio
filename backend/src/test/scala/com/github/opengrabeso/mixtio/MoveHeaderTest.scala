package com.github.opengrabeso.mixtio

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Created by Ondra on 23.3.2016.
  */
class MoveHeaderTest extends AnyFlatSpec with Matchers {
  import MoveHeader._

  behavior of "MoveHeaderTest"

  val quest = "Suunto Quest"
  val gps = "Suunto GPS Track Pod"
  val foot = "Suunto Foot Pod Mini"

  it should "merge identical names" in {
    mergeDeviceNames(Set(gps, gps)) shouldBe Some(gps)
    mergeDeviceNames(Set(quest, quest)) shouldBe Some(quest)
  }

  it should "merge different names" in {
    val questAndGPS = "Suunto Quest + GPS Track Pod"
    val questAndFootAndGPS = "Suunto Quest + GPS Track Pod + Foot Pod Mini"
    mergeDeviceNames(Set(gps, quest)) shouldBe Some(gps)
    mergeDeviceNames(Set(quest, gps)) shouldBe Some(gps)
    mergeDeviceNames(Set(quest, foot, gps)) shouldBe Some(gps)
    mergeDeviceNames(Set(quest, gps, foot)) shouldBe Some(gps)
    /*
    mergeDeviceNames(Set(gps, quest)) shouldBe Some(questAndGPS)
    mergeDeviceNames(Set(quest, gps)) shouldBe Some(questAndGPS)
    mergeDeviceNames(Set(quest, foot, gps)) shouldBe Some(questAndFootAndGPS)
    mergeDeviceNames(Set(quest, gps, foot)) shouldBe Some(questAndFootAndGPS)
    */
  }
}

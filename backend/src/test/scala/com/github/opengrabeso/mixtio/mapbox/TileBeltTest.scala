package com.github.opengrabeso.mixtio.mapbox

import org.scalatest.funsuite.AnyFunSuite

class TileBeltTest extends AnyFunSuite {

  /* ScalaFromJS: 2017-10-23 09:41:46.284*/

  val tile1 = Array[Int](5, 10, 10)
  val tilebelt = TileBelt

  test("tile to geojson") {
    val geojson = tilebelt.tileToGeoJSON(tile1)
    assert(geojson.coordinates.nonEmpty)
  }
  test("tile to bbox") {
    val ext = tilebelt.tileToBBOX(tile1)
    assert(ext != null)
    assert(ext.toSeq == Array(-178.2421875, 84.7060489350415, -177.890625, 84.73838712095339).toSeq)

  }
  test("get parent") {
    val parent = tilebelt.getParent(tile1)
    assert(parent.length == 3)
    assert(parent(0) == 2)
    assert(parent(1) == 5)
    assert(parent(2) == 9)

  }
  test("get siblings") {
    val siblings = tilebelt.getSiblings(tile1)
    assert(siblings.length ==  4)
    assert(siblings(0).length == 3)

  }
  test("has siblings") {
    val tiles1 = Array(Array(0, 0, 5), Array(0, 1, 5), Array(1, 1, 5), Array(1, 0, 5))
    val tiles2 = Array(Array(0, 0, 5), Array(0, 1, 5), Array(1, 1, 5))
    assert(tilebelt.hasSiblings(Array(0, 0, 5), tiles1))
    assert(tilebelt.hasSiblings(Array(0, 1, 5), tiles1))
    assert(!tilebelt.hasSiblings(Array(0, 0, 5), tiles2))
    assert(!tilebelt.hasSiblings(Array(0, 0, 5), tiles2))

  }
  test("has tile") {
    val tiles1 = Array(Array(0, 0, 5), Array(0, 1, 5), Array(1, 1, 5), Array(1, 0, 5))
    assert(!tilebelt.hasSiblings(Array(2, 0, 5), tiles1))
    assert(tilebelt.hasSiblings(Array(0, 1, 5), tiles1))

  }
  test("get quadkey") {
    val key = tilebelt.tileToQuadkey(Array(11, 3, 8))
    assert(key == "00001033")

  }
  test("quadkey to tile") {
    val quadkey = "00001033"
    val tile = tilebelt.quadkeyToTile(quadkey)
    assert(tile.length == 3)

  }
  test("point to tile") {
    val tile = tilebelt.pointToTile(0, 0, 10)
    assert(tile.length == 3)
    assert(tile(2) == 10)

  }
  test("point to tile verified") {
    val tile = tilebelt.pointToTile(-77.03239381313323, 38.91326516559442, 10)
    assert(tile.length == 3)
    assert(tile(0) == 292)
    assert(tile(1) == 391)
    assert(tile(2) ==  10)
    assert(tilebelt.tileToQuadkey(tile) == "0320100322")

  }
  test("point and tile back and forth") {
    val tile = tilebelt.pointToTile(10, 10, 10)
    assert(tile.toSeq == tilebelt.quadkeyToTile(tilebelt.tileToQuadkey(tile)).toSeq)

  }
  test("check key 03") {
    val quadkey = "03"
    assert(tilebelt.quadkeyToTile(quadkey).toSeq == Array(1, 1, 2).toSeq)

  }
  test("bbox to tile -- big") {
    val bbox = Array(-84.72656249999999, 11.178401873711785, -5.625, 61.60639637138628)
    val tile = tilebelt.bboxToTile(bbox)
    assert(tile(0) == 1)
    assert(tile(1) == 1)
    assert(tile(2) == 2)

  }
  test("bbox to tile -- no area") {
    val bbox = Array[Double](-84, 11, -84, 11)
    val tile = tilebelt.bboxToTile(bbox)
    assert(tile.toSeq ==  Array(71582788, 125964677, 28).toSeq)

  }
  test("bbox to tile -- dc") {
    val bbox = Array(-77.04615354537964, 38.899967510782346, -77.03664779663086, 38.90728142481329)
    val tile = tilebelt.bboxToTile(bbox)
    assert(tile(0) == 9371)
    assert(tile(1) == 12534)
    assert(tile(2) == 15)

  }
  test("bbox to tile -- crossing 0 lat/lng") {
    val bbox = Array[Double](-10, -10, 10, 10)
    val tile = tilebelt.bboxToTile(bbox)
    assert(tile(0) == 0)
    assert(tile(1) == 0)
    assert(tile(2) == 0)

  }
  test("tile to bbox -- verify bbox order") {
    var tile = Array(13, 11, 5)
    var bbox = tilebelt.tileToBBOX(tile)
    assert(bbox(0) < bbox(2), "east is less than west")
    assert(bbox(1) < bbox(3), "south is less than north")
    tile = Array(20, 11, 5)
    bbox = tilebelt.tileToBBOX(tile)
    assert(bbox(0) < bbox(2), "east is less than west")
    assert(bbox(1) < bbox(3), "south is less than north")
    tile = Array(143, 121, 8)
    bbox = tilebelt.tileToBBOX(tile)
    assert(bbox(0) < bbox(2), "east is less than west")
    assert(bbox(1) < bbox(3), "south is less than north")
    tile = Array(999, 1000, 17)
    bbox = tilebelt.tileToBBOX(tile)
    assert(bbox(0) < bbox(2), "east is less than west")
    assert(bbox(1) < bbox(3), "south is less than north")

  }
  test("pointToTileFraction") {
    val tile = tilebelt.pointToTileFraction(-95.93965530395508, 41.26000108568697, 9)
    assert(tile(0) == 119.552490234375)
    assert(tile(1) == 191.47119140625)
    assert(tile(2) == 9)

  }

}

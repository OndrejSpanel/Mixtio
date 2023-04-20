package com.github.opengrabeso.mixtio.facade

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation._

import GeoJSON._

@JSGlobal
@js.native
object mapboxgl extends js.Any {
  var accessToken: String = js.native

  // https://docs.mapbox.com/mapbox-gl-js/api/#lnglatlike
  @js.native
  class LngLat(var lng: Double = js.native, var lat: Double = js.native) extends js.Object

  @js.native
  class LngLatBounds(var _ne: LngLat = js.native, var _sw: LngLat = js.native) extends js.Object {
    def getNorth(): Double = js.native
    def getSouth(): Double = js.native
    def getEast(): Double = js.native
    def getWest(): Double = js.native
  }


  @js.native // https://github.com/mapbox/mapbox-gl-js/blob/master/flow-typed/point-geometry.js
  class Point(val x: Double, val y: Double) extends js.Object {

  }

  @js.native // https://github.com/mapbox/mapbox-gl-js/blob/master/src/util/evented.js
  class Event(val `type`: String, data: js.Object = js.native) extends js.Object {

  }

  @js.native // https://github.com/mapbox/mapbox-gl-js/blob/master/src/ui/events.js
  class MapMouseEvent(`type`: String, data: js.Object = js.native) extends Event(`type`, data) {
    val target: Map = js.native
    val originalEvent: dom.MouseEvent = js.native
    val point: Point = js.native
    val lngLat: LngLat = js.native
  }

  @js.native // https://github.com/mapbox/mapbox-gl-js/blob/master/src/util/evented.js
  class Evented extends js.Object {
    def on(event: String, callback: js.Function1[Event, Unit]): Unit = js.native
    def off(event: String, callback: js.Function1[Event, Unit]): Unit = js.native
  }

  @js.native // https://github.com/mapbox/mapbox-gl-js/blob/master/src/ui/map.js
  class Map(options: js.Object) extends Evented {

    def queryRenderedFeatures(geometry: Point, options: js.Object): js.Array[Feature] = js.native
    def queryRenderedFeatures(geometry: js.Array[Point], options: js.Object): js.Array[Feature] = js.native
    def queryRenderedFeatures(options: js.Object = js.native): js.Array[Feature] = js.native

    def addSource(name: String, content: js.Object): Unit = js.native
    def addLayer(layer: js.Object): Unit = js.native
    def getContainer(): dom.Element = js.native
    def getSource(name: String): js.UndefOr[js.Dynamic] = js.native
    def getBounds(): LngLatBounds = js.native
    def getCanvas(): dom.HTMLCanvasElement = js.native
    def setPaintProperty(name1: String, name2: String, value: js.Any): Unit = js.native
    def setLayoutProperty(name1: String, name2: String, value: js.Any): Unit = js.native
    def fitBounds(bounds: LngLatBounds, options: js.Object = js.native, eventData: js.Object = js.native): Unit = js.native
    def unproject(p: Point): LngLat = js.native
  }

  @js.native // https://github.com/mapbox/mapbox-gl-js/blob/master/src/ui/popup.js
  class Popup extends js.Object {
    def remove(): Unit = js.native
    def setLngLat(lngLat: LngLat): Popup = js.native
    def setHTML(html: String): Popup = js.native
    def setText(html: String): Popup = js.native
    def setDOMContent(node: dom.Node): Popup = js.native
    def addTo(map: Map): Popup = js.native
  }
}

package com.github.opengrabeso.mixtio
package frontend.views

import common.Formatting
import common.model._
import frontend.model._
import io.udash.component.ComponentId
import org.scalajs.dom
import scalatags.JsDom.all.{select => tag_select, _}
import io.udash._
import org.scalajs.dom.HTMLOptionElement

import scala.scalajs.js.annotation.JSExportTopLevel

object EventView {
  implicit class Text(s: String) {
    def text = span(s).render
  }

  def segmentTitle(kind: String, e: SegmentTitle): dom.Element = {
    val segPrefix = if (e.isPrivate) "private " else ""
    val segmentName = Formatting.shortNameString(e.name, 32 - segPrefix.length - kind.length)
    if (e.segmentId != 0) {
      span(
        (kind + segPrefix).capitalize,
        a(
          title := e.name,
          href := s"https://www.strava.com/segments/${e.segmentId}",
          segmentName
        )
      ).render
    } else {
      (kind + segPrefix + segmentName).capitalize.text
    }
  }

  def eventDescription(e: EditEvent): dom.Element = {
    e.event match {
      case e: PauseEvent =>
        s"Pause ${Events.niceDuration(e.duration)}".text
      case e: PauseEndEvent =>
        "Pause end".text
      case e: LapEvent =>
        "Lap".text
      case e: EndEvent =>
        "End".text
      case e: BegEvent =>
        b("Start").render
      case e: SplitEvent =>
        "Split".text
      case e: StartSegEvent =>
        segmentTitle("", e)
      case e: EndSegEvent =>
        segmentTitle("end ", e)
      case e: ElevationEvent =>
        Formatting.shortNameString("Elevation " + e.elev.toInt + " m").text
    }

  }

  def selectId(time: Int): ComponentId = ComponentId("actionSelect").withSuffix(time.toString)

  def getSelectHtml(editEvent: EditEvent, title: String): dom.Element = {
    // replicate the option used in the table
    val tableOption = dom.document.getElementById(selectId(editEvent.time).toString)
    val html = tableOption.innerHTML

    span(
      title,
      br,
      tag_select(
        raw(html),
        onchange :+= { e: dom.Event =>
          val target = e.target.asInstanceOf[HTMLOptionElement]
          onChangeEvent(target.value, editEvent.time)
          false
        }

      )
    ).render
  }

  @JSExportTopLevel("onChangeEvent")
  def onChangeEvent(value: String, time: Int): Unit = {
    val option = dom.document.getElementById(selectId(time).toString).asInstanceOf[HTMLOptionElement]
    option.value = value
  }
}

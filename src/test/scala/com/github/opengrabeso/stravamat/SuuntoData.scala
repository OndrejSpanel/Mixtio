package com.github.opengrabeso.stravamat

import org.joda.time.DateTimeZone

import scala.io.Source
import scala.util.Try
import scala.xml.XML
import scala.xml.pull.XMLEventReader

trait SuuntoData {
  protected def gpsPodMove: Try[Move] = {
    val res = getClass.getResourceAsStream("/suuntoMerge/Moveslink2/gps.sml")

    val doc = new XMLEventReader(Source.fromInputStream(res))
    val move = moveslink2.XMLParser.parseXML("gps.sml", doc)
    move
  }

  protected def questMove: Seq[Move] = {
    val res = getClass.getResourceAsStream("/suuntoMerge/Moveslink/quest.xml")

    val doc = new XMLEventReader(Source.fromInputStream(moveslink.XMLParser.skipMoveslinkDoctype(res)))
    val localTimeZone = DateTimeZone.getDefault.toString

    val move = moveslink.XMLParser.parseXML("quest.xml", doc, 240, localTimeZone)
    move
  }


}

package com.github.opengrabeso.mixtio
package common.model

import com.avsystem.commons.rpc.AsRawReal
import io.udash.rest.raw.{HttpBody, IMapping, PlainValue, RestResponse}

case class BinaryData(data: Array[Byte])

// create more efficient encoding
// otherwise the Array is serialized to JSON, producing signed decimal representation of bytes delimited by commas

object BinaryData extends rest.EnhancedRestDataCompanion[BinaryData] {
  private val contentType = "application/octet-stream"
  implicit val rawReal: AsRawReal[RestResponse, BinaryData] = AsRawReal.create(
    real => RestResponse(200, IMapping.create[PlainValue](), HttpBody.binary(real.data, contentType)),
    raw => BinaryData(raw.body.readBytes(contentType))
  )

}

package com.github.opengrabeso.mixtio
package requests

import org.apache.commons.io.IOUtils

object StaticContent extends DefineRequest("/static/*") {
  def html(request: Request, response: Response) = {
    val filename = request.url
    val stream = getClass.getResourceAsStream(filename)
    if (stream != null) {
      try {
        val out = response.getOutputStream
        IOUtils.copy(stream, out)
        out.close()
      } finally {
        stream.close()
      }
    } else {
      response.status(404)
    }
    Seq.empty
  }
}


package com.github.opengrabeso.mixtio
package requests

import com.github.opengrabeso.mixtio.shared.Timing
import common.css._
import io.udash.css.CssStringRenderer
import scalacss.internal.{Renderer, StringRenderer}
import org.apache.commons.io.IOUtils

/*
Doing CSS rendering runtime is much simpler. As long as CSS is not very complicated, it should be reasonably fast (~10 ms).
For compile time (SBT based) rendering see commit 682a1f6c72c2135c0e0bf0dbb383ccf7e06f8427
*/

object FrontendStyle extends DefineRequest("/frontend/main.css") {
  private val styles = Seq(
    GlobalStyles,
    SelectPageStyles,
    SettingsPageStyles,
    EditPageStyles
  )
  def html(request: Request, resp: Response) = {

    val timing = Timing.start(true)

    implicit val renderer: Renderer[String] = StringRenderer.defaultPretty
    val cssString = new CssStringRenderer(styles).render()

    timing.logTime(s"CSS render")

    resp.status(200)
    resp.`type`("text/css")

    val out = resp.getOutputStream
    IOUtils.write(cssString, out)
    IOUtils.write("\n", out) // prevent empty file by always adding an empty line, empty file not handled well by Spark framework

    Nil
  }
}

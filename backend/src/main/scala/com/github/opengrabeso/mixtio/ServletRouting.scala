package com.github.opengrabeso.mixtio

import com.github.opengrabeso.mixtio.DevServer.DefRoute
import spark.{Request, Response, Route}
import spark.servlet.SparkApplication
import spark.Spark._

object ServletRouting {
  import requests._

  def route(path: String)(handleFunc: (Request, Response) => AnyRef): Route = {
    new DefRoute(path) {
      override def handle(request: Request, response: Response) = handleFunc(request, response)
    }
  }

  val handlers: Seq[DefineRequest] = Seq(
    IndexHtml,

    FrontendStyle,
    FrontendScript, UdashApp,

    Cleanup
  )

  def init() {
    // add any type derived from DefineRequest here
    // solution with reflection is elegant, but overcomplicated (and hard to get working with Google App Engine) and slow
    def addPage(h: DefineRequest) = {
      h.method match {
        case Method.Get => get(h.handleUri, h)
        case Method.Put => put(h.handleUri, h)
        case Method.Post => post(h.handleUri, h)
        case Method.Delete => delete(h.handleUri, h)
      }
      println(s"Defined route ${h.handleUri}")
    }

    handlers.foreach(addPage)
  }
}

class ServletRouting extends SparkApplication {

  def init() = {

    ServletRouting.init()

  }


}

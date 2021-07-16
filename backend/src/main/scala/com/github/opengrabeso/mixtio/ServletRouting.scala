package com.github.opengrabeso.mixtio

import spark.{Request, Response, Route}
import spark.servlet.SparkApplication
import spark.Spark._

object ServletRouting {
  import requests._

  def route(path: String)(handleFunc: (Request, Response) => AnyRef): Route = {
    new Route(path) {
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
      val r = route(h.handleUri) (h.apply)
      h.method match {
        case Method.Get => get(r)
        case Method.Put => put(r)
        case Method.Post => post(r)
        case Method.Delete => delete(r)
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

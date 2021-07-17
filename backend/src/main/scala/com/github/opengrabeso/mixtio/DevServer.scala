package com.github.opengrabeso.mixtio

import org.apache.commons.io.IOUtils
import spark.{Request, Response, Route}
import spark.Spark.{connect, delete, get, halt, head, options, patch, port, post, put, trace}
import spark.embeddedserver.jetty.HttpRequestWrapper

object DevServer {
  val portNumber = System.getenv.getOrDefault("PORT", "8080").toInt
  val GAE_APPLICATION = System.getenv.get("GAE_APPLICATION")
  val GAE_ENV = System.getenv.get("GAE_ENV")
  val GAE_RUNTIME = System.getenv.get("GAE_RUNTIME")

  abstract class DefRoute(val path: String) extends Route

  def main(args: Array[String]): Unit = {
    // start embedded Spark / Jetty server
    // defining routing will start init on its own

    println(s"Starting Spark at port $portNumber, environment $GAE_ENV")
    port(portNumber)

    object RestRoute extends DefRoute("/rest/*") {
      object servlet extends rest.ServletRestAPIRest
      def handle(request: Request, response: Response) = {
        servlet.service(request.raw, response.raw)
        response
      }
    }

    object StaticRoute extends DefRoute("/static/*") {
      def handle(request: Request, response: Response) = {
        val filename = "/static/" + request.splat().mkString("/")
        val stream = getClass.getResourceAsStream(filename)
        if (stream != null) {
          try {
            val out = response.raw.getOutputStream
            IOUtils.copy(stream, out)
            out.close()
          } finally {
            stream.close()
          }
        } else {
          halt(404, s"Static $filename not found")
        }
        response
      }
    }

    get(RestRoute.path, RestRoute)
    post(RestRoute.path, RestRoute)
    put(RestRoute.path, RestRoute)
    delete(RestRoute.path, RestRoute)
    patch(RestRoute.path, RestRoute)
    head(RestRoute.path, RestRoute)
    connect(RestRoute.path, RestRoute)
    options(RestRoute.path, RestRoute)
    trace(RestRoute.path, RestRoute)

    get(StaticRoute.path, StaticRoute)

    ServletRouting.init()
  }

}

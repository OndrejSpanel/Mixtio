package com.github.opengrabeso.mixtio

import java.io.{File, FileInputStream}

import org.apache.commons.io.IOUtils
import spark.{Request, Response, Route}
import spark.Spark.{connect, delete, get, head, options, patch, post, put, trace}

object DevServer {
  /** Note: server started this way has some limitations
    * */
  def main(args: Array[String]): Unit = {
    // start embedded Spark / Jetty server
    // defining routing will start init on its own

    object RestRoute extends Route("/rest/*") {
      object servlet extends rest.ServletRestAPIRest
      def handle(request: Request, response: Response) = {
        // maybe there is some more portable way, currently we do servlet path change only for Jetty requests
        // when running embedded, we are sure to be running Jetty, therefore this should work fine
        request.raw match {
          case req: org.eclipse.jetty.server.Request =>
            val pathInfo = req.getPathInfo
            if (pathInfo.startsWith("/rest")) {
              req.setServletPath("/rest")
            }
          case _ =>
        }
        servlet.service(request.raw, response.raw)
        response
      }
    }

    object StaticRoute extends Route("/static/*") {
      def handle(request: Request, response: Response) = {
        val filename = request.splat().head
        val path = "backend/web/static/" + filename
        val stream = new FileInputStream(new File(path))
        try {
          val out = response.raw.getOutputStream
          IOUtils.copy(stream, out)
          out.close()
        } finally {
          stream.close()
        }
        response
      }
    }

    get(RestRoute)
    post(RestRoute)
    put(RestRoute)
    delete(RestRoute)
    patch(RestRoute)
    head(RestRoute)
    connect(RestRoute)
    options(RestRoute)
    trace(RestRoute)

    get(StaticRoute)

    ServletRouting.init()
  }

}

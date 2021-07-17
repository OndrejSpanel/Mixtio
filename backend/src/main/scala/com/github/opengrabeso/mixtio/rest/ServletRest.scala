package com.github.opengrabeso.mixtio
package rest

import java.io.ByteArrayOutputStream

import com.avsystem.commons._
import io.udash.rest.RestServlet
import io.udash.rest.RestServlet.CookieHeader
import io.udash.rest.raw.{HttpBody, HttpErrorException, HttpMethod, IMapping, Mapping, PlainValue, RawRest, RestParameters, RestRequest, RestResponse}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import scala.annotation.tailrec
import scala.util.{Failure, Success}

trait ReadRequest {
  def maxPayloadSize: Long
  def BufferSize: Int

  def readParameters(request: HttpServletRequest): RestParameters = {
    // can't use request.getPathInfo because it decodes the URL before we can split it
    val pathPrefix = request.getContextPath.orEmpty + request.getServletPath.orEmpty
    val path = PlainValue.decodePath(request.getRequestURI.stripPrefix(pathPrefix))
    val query = request.getQueryString.opt.map(PlainValue.decodeQuery).getOrElse(Mapping.empty)
    val headersBuilder = IMapping.newBuilder[PlainValue]
    request.getHeaderNames.asInstanceOf[java.util.Enumeration[String]].asScala.foreach { headerName =>
      if (!headerName.equalsIgnoreCase(CookieHeader)) { // cookies are separate, don't include them into header params
        headersBuilder += headerName -> PlainValue(request.getHeader(headerName))
      }
    }
    val headers = headersBuilder.result()
    val cookiesBuilder = Mapping.newBuilder[PlainValue]
    request.getCookies.opt.getOrElse(Array.empty).foreach { cookie =>
      cookiesBuilder += cookie.getName -> PlainValue(cookie.getValue)
    }
    val cookies = cookiesBuilder.result()
    RestParameters(path, headers, query, cookies)
  }

  def readBody(request: HttpServletRequest): HttpBody = {
    val contentLength = request.getContentLength.opt.filter(_ != -1)
    contentLength.filter(_ > maxPayloadSize).foreach { length =>
      throw HttpErrorException(413, s"Payload is larger than maximum $maxPayloadSize bytes ($length)")
    }

    request.getContentType.opt.fold(HttpBody.empty) { contentType =>
      val mediaType = HttpBody.mediaTypeOf(contentType)
      HttpBody.charsetOf(contentType) match {
        // if Content-Length is undefined, always read as binary in order to validate maximum length
        case Opt(charset) if contentLength.isDefined =>
          val bodyReader = request.getReader
          val bodyBuilder = new JStringBuilder
          val cbuf = new Array[Char](BufferSize)
          @tailrec def readLoop(): Unit = bodyReader.read(cbuf) match {
            case -1 =>
            case len =>
              bodyBuilder.append(cbuf, 0, len)
              readLoop()
          }
          readLoop()
          HttpBody.textual(bodyBuilder.toString, mediaType, charset)

        case _ =>
          val bodyIs = request.getInputStream
          val bodyOs = new ByteArrayOutputStream
          val bbuf = new Array[Byte](BufferSize)
          @tailrec def readLoop(): Unit = bodyIs.read(bbuf) match {
            case -1 =>
            case len =>
              bodyOs.write(bbuf, 0, len)
              if (bodyOs.size > maxPayloadSize) {
                throw HttpErrorException(413, s"Payload is larger than maximum $maxPayloadSize bytes")
              }
              readLoop()
          }
          readLoop()
          HttpBody.binary(bodyOs.toByteArray, contentType)
      }
    }
  }

  def readRequest(request: HttpServletRequest): RestRequest = {
    val method = HttpMethod.byName(request.getMethod)
    val parameters = readParameters(request)
    val body = readBody(request)
    RestRequest(method, parameters, body)
  }
}

trait WriteResponse {
  def writeResponse(response: HttpServletResponse, restResponse: RestResponse): Unit = {
    response.setStatus(restResponse.code)
    restResponse.headers.entries.foreach {
      case (name, PlainValue(value)) => response.addHeader(name, value)
    }
    restResponse.body match {
      case HttpBody.Empty =>
      case neBody: HttpBody.NonEmpty =>
        // TODO: can we improve performance by avoiding intermediate byte array for textual content?
        val bytes = neBody.bytes
        response.setContentType(neBody.contentType)
        response.setContentLength(bytes.length)
        response.getOutputStream.write(bytes)
    }
  }

  def writeFailure(response: HttpServletResponse, message: Opt[String]): Unit = {
    response.setStatus(500)
    message.foreach { msg =>
      response.setContentType(s"text/plain;charset=utf-8")
      response.getWriter.write(msg)
    }
  }
}

class ServletRestAPIRest extends RestServlet(RawRest.asHandleRequest[RestRootAPI](RestRootAPIServer))


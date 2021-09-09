package com.github.opengrabeso.mixtio

import javax.servlet.{ServletRequest, ServletResponse}
import javax.servlet.http.{Cookie, HttpServletRequest, HttpServletResponse}

trait ServletUtils {
  implicit class ServletRequestOps(req: ServletRequest) {
    def http: HttpServletRequest = req.asInstanceOf[HttpServletRequest]
    def queryString: String = http.getQueryString
    def queryParams(name: String): String = req.getParameter(name)
    def url: String = http.getRequestURL.toString
    def relativeUrl: String = http.getRequestURI
    def cookie(name: String): String = {
      val cookieWithName = for {
        cookies <- Option(http.getCookies).toSeq
        c <- cookies
        if c.getName == name
      } yield {
        c.getValue
      }
      cookieWithName.headOption.orNull
    }
  }

  implicit class ServletResponseOps(resp: ServletResponse) {
    def http: HttpServletResponse = resp.asInstanceOf[HttpServletResponse]
    def status(code: Int): Unit = http.setStatus(code)
    def redirect(url: String): Unit = {
      http.sendRedirect(http.encodeRedirectURL(url))
    }
    def cookie(name: String, value: String, expire: Int = -1): Unit = {
      val c = new Cookie(name, value)
      c.setMaxAge(expire)
      http.addCookie(c)
    }
    def removeCookie(name: String): Unit = cookie(name, "", 0)
    def `type`(mime: String): Unit = resp.setContentType(mime)
  }
}

object ServletUtils extends ServletUtils
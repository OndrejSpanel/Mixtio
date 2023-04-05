package com.github.opengrabeso.mixtio

import java.net.{URLDecoder, URLEncoder}
import Main._

import javax.servlet.{ServletRequest, ServletResponse}
import scala.util.Try
import scala.xml.NodeSeq
import ServletUtils._

import java.nio.charset.StandardCharsets

sealed trait Method
object Method {
  case object Get extends Method
  case object Put extends Method
  case object Post extends Method
  case object Delete extends Method

}

case class Handle(value: String, method: Method = Method.Get)

object DefineRequest {
  abstract class Post(handleUri: String) extends DefineRequest(handleUri, method = Method.Post)
}

abstract class DefineRequest(val handleUri: String, val method: Method = Method.Get) extends ServletUtils {
  type Request = ServletRequest
  type Response = ServletResponse

  def uriRest(request: ServletRequest): String = {
    val uri = request.relativeUrl
    if (handleUri.endsWith("*")) {
      val prefix = handleUri.dropRight(1)
      assert(uri.startsWith(prefix))
      uri.drop(prefix.length)
    } else {
      throw new UnsupportedOperationException(s"Cannot get URI rest by pattern $handleUri")
    }

  }

  // some actions (logout) may have their URL prefixed to provide a specific functionality

  def handle(request: Request, resp: Response): Unit = {

    val nodes = html(request, resp)
    if (nodes.nonEmpty) {
      nodes.head match {
        case <html>{_*}</html> =>
          val docType = """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd" >"""
          val body = docType + nodes.toString
          resp.`type`("text/html")
          resp.getOutputStream.write(body.getBytes(StandardCharsets.UTF_8))
        case _ =>
          resp.setContentType("text/xml; charset=utf-8")
          val xmlPrefix = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n"
          val body = xmlPrefix + nodes.toString
          resp.getOutputStream.write(body.getBytes(StandardCharsets.UTF_8))
          resp.`type`("text/xml")
      }
    }
  }

  def html(request: Request, resp: Response): NodeSeq

  def cond(boolean: Boolean) (nodes: NodeSeq): NodeSeq = {
    if (boolean) nodes else Nil
  }

  def headPrefix: NodeSeq = {
    <meta charset="utf-8"/>
    <link rel="icon" href="static/favicon.ico"/>
  }

  def bodyHeader(auth: Main.StravaAuthResult): NodeSeq = {
    <div id="header" style="background-color:#fca;overflow:auto">
    <table>
      <tr>
        <td>
          <a href="/"><img src="static/stravaUpload64.png"></img></a>
        </td>
        <td>
          <table>
            <tr>
              <td>
                <a href="/">{appName}</a>
              </td>
            </tr>
            <tr>
              <td>
                Athlete:
                <a href={s"https://www.strava.com/athletes/${auth.id}"}>
                  {auth.name}
                </a>
              </td>
            </tr>
          </table>
        </td>
        <td>
        <form action={"logout"}>
          <input type="submit" value ="Log Out"/>
        </form>
        </td>
      </tr>
    </table>
    </div>
    <p></p>
  }

  def bodyFooter: NodeSeq = {
    <p></p>
    <div id="footer" style="background-color:#fca;overflow:auto">
      <a href="http://labs.strava.com/" id="powered_by_strava" rel="nofollow">
        <img align="left" src="static/api_logo_pwrdBy_strava_horiz_white.png" style="max-height:46px"/>
      </a>
      <p style="color:#fff">© 2016 - 2018 <a href={s"https://github.com/OndrejSpanel/${gitHubName}"} style="color:inherit">Ondřej Španěl</a></p>
      <div/>
    </div>
  }

  def storeAuthCookies(resp: ServletResponse, auth: StravaAuthResult) = {
    println(s"Store auth cookies session ${auth.sessionId}")
    val loginAge = 3600 * 24 * 30 // 30 days
    resp.cookie("authToken", auth.token, loginAge)
    resp.cookie("authRefreshToken", auth.refreshToken, loginAge)
    resp.cookie("authRefreshExpire", auth.refreshExpire.toString, loginAge)
    resp.cookie("authUserName", URLEncoder.encode(auth.name, "UTF-8"), loginAge)
    resp.cookie("authUserId", auth.userId, loginAge)
    resp.cookie("sessionId", auth.sessionId) // session cookie - no expiry
  }

  def performAuth(code: String, resp: ServletResponse): Try[StravaAuthResult] = {
    val authResult = Try(Main.stravaAuth(code))
    authResult.foreach { auth =>
      println("Login done, create authCode cookie")
      storeAuthCookies(resp, auth)
    }
    if (authResult.isFailure) {
      println("Strava authentication failed")
    }
    authResult
  }

  def loadAuthFromCookies(r: ServletRequest): Option[StravaAuthResult] = {
    val token = r.cookie("authToken")
    val refresh = r.cookie("authRefreshToken")
    val refreshExpire = r.cookie("authRefreshExpire")
    val userId = r.cookie("authUserId")
    val userName = r.cookie("authUserName")
    if (token != null && refresh != null && refreshExpire != null && userName != null && userId != null) {
      Some {
        StravaAuthResult(
          token, refresh, refreshExpire.toLong,
          mapboxToken = secret.mapboxToken, id = userId,
          name = URLDecoder.decode(userName, "UTF-8"),
          sessionId = "full-session-" + System.currentTimeMillis().toString
        )
      }
    } else {
      None
    }
  }

  def withAuth(req: ServletRequest, resp: ServletResponse)(body: StravaAuthResult => NodeSeq): NodeSeq = {
    val codePar = Option(req.queryParams("code"))
    val statePar = Option(req.queryParams("state")).filter(_.nonEmpty)
    codePar.fold {
      val auth = loadAuthFromCookies(req).map(stravaAuthRefresh)
      auth.map { a =>
        storeAuthCookies(resp, a)
        body(a)
      }.getOrElse {
        println("withAuth loginPage")
        loginPage(req, resp, req.url, Option(req.queryString))
      }
    } { code =>
      if (performAuth(code, resp).isSuccess) {
        println("withAuth performAuth redirect")
        resp.redirect(req.url + statePar.fold("")("?" + _))
        NodeSeq.Empty
      } else {
        println("withAuth performAuth loginPage")
        loginPage(req, resp, req.url, statePar)
      }
    }
  }

  def loginPage(request: Request, resp: Response, afterLogin: String, afterLoginParams: Option[String]): NodeSeq = {
    if (request.cookie("authToken") != null) {
      println("Login page, delete authCode cookie")
    } else {
      println("Login page, no authToken cookie")
    }

    resp.removeCookie("authToken")
    resp.removeCookie("authRefreshToken")
    resp.removeCookie("authRefreshExpire")
    resp.removeCookie("authUserName")
    resp.removeCookie("authUserId")

    resp.removeCookie("authCode") // obsolete, but we delete it anyway
    <html>
      <head>
        {headPrefix}
        <title>{appName}</title>
      </head>
      <body>
        {
        val secret = Main.secret
        val clientId = secret.appId
        val uri = "https://www.strava.com/oauth/authorize?"
        val state = afterLoginParams.fold("")(pars => "&state=" + URLEncoder.encode(pars, "UTF-8"))
        val action = uri + "client_id=" + clientId + "&response_type=code&redirect_uri=" + afterLogin + state + "&scope=read,activity:read_all,activity:write&approval_prompt=force"
        <h3>Work in progress, use at your own risk.</h3>
          <p>
            Strava activity editing (including uploading, merging and splitting)
          </p>
          <ul>
            <li>Merge activities</li>
            <li>Edit lap information</li>
            <li>Show activity map</li>
            <li>Split activities</li>
          </ul> :+ {
          if (clientId.nonEmpty) {
            <a href={action}>
              <img src="static/ConnectWithStrava.png" alt="Connect with STRAVA"></img>
            </a>
          } else {
            <p>Error:
              {secret.error}
            </p>
          }
        }
        }
        {bodyFooter}
      </body>
    </html>
  }

}

package com.github.opengrabeso.mixtio

import java.net.{URLDecoder, URLEncoder}
import Main._
import spark.{Request, Response}

import scala.util.Try
import scala.xml.NodeSeq

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

abstract class DefineRequest(val handleUri: String, val method: Method = Method.Get) {

  // some actions (logout) may have their URL prefixed to provide a specific functionality

  def apply(request: Request, resp: Response): AnyRef = {

    import com.google.appengine.api.utils.SystemProperty

    if (SystemProperty.environment.value() == SystemProperty.Environment.Value.Development) {
      // logging on production server is counter-productive, logs are already sorted by request
      println(s"Request ${request.url()}")
    }
    val nodes = html(request, resp)
    if (nodes.nonEmpty) {
      nodes.head match {
        case <html>{_*}</html> =>
          val docType = """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd" >"""
          docType + nodes.toString
        case _ =>
          resp.`type`("text/xml; charset=utf-8")
          val xmlPrefix = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n"
          xmlPrefix + nodes.toString
      }
    } else resp
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
      <p style="color:#fff"><a href="https://darksky.net/poweredby/" style="color:#fff">Powered by Dark Sky</a> © 2016 - 2018 <a href={s"https://github.com/OndrejSpanel/${gitHubName}"} style="color:inherit">Ondřej Španěl</a></p>
      <div/>
    </div>
  }

  def storeAuthCookies(resp: Response, auth: StravaAuthResult) = {
    println(s"Store auth cookies session ${auth.sessionId}")
    val loginAge = 3600 * 24 * 30 // 30 days
    resp.cookie("authToken", auth.token, loginAge)
    resp.cookie("authRefreshToken", auth.refreshToken, loginAge)
    resp.cookie("authRefreshExpire", auth.refreshExpire.toString, loginAge)
    resp.cookie("authUserName", URLEncoder.encode(auth.name, "UTF-8"), loginAge)
    resp.cookie("authUserId", auth.userId, loginAge)
    resp.cookie("sessionId", auth.sessionId) // session cookie - no expiry
  }

  def performAuth(code: String, resp: Response): Try[StravaAuthResult] = {
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

  def loadAuthFromCookies(r: Request): Option[StravaAuthResult] = {
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

  def withAuth(req: Request, resp: Response)(body: StravaAuthResult => NodeSeq): NodeSeq = {
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
        resp.redirect(req.url() + statePar.fold("")("?" + _))
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

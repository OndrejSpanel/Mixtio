package com.github.opengrabeso.mixtio
package requests
package push

import java.net.URLEncoder

import spark.{Request, Response}

import scala.xml.NodeSeq
import java.time.ZonedDateTime
import common.Util._

object PushStart extends DefineRequest("/push-start") {
  override def showSuuntoUploadInstructions = false // no need to show this, user already launched it

  def html(req: Request, resp: Response) = withAuth(req, resp) { auth =>
    val session = req.session()
    // We need the ID to be unique for a given user, timestamps seems reasonable for this.
    // Normal web app session ID is not unique, sessions get reused.
    val sessionId = req.queryParams("session")
    val port = req.queryParams("port").toInt

    val stravaActivities = Main.recentStravaActivities(auth)
    session.attribute("stravaActivities", stravaActivities)

    // ignore anything older than oldest of recent Strava activities
    val ignoreBeforeLast = stravaActivities.lastOption.map(_.startTime) // oldest of the last 15 Strava activities
    val ignoreBeforeFirst = stravaActivities.headOption.map(_.startTime minusDays  14) // most recent on Strava - 2 weeks
    val ignoreBeforeNow = ZonedDateTime.now() minusMonths 2 // max. 2 months

    val since = (Seq(ignoreBeforeNow) ++ ignoreBeforeLast ++ ignoreBeforeFirst).max

    def encode(x: String) = URLEncoder.encode(x, "UTF-8")

    // hotfix - sometimes the cookies seem missing after withAuth
    println(s"Setting authCode cookie ${auth.code.length}")
    println(s"Setting authCode sessionId ${auth.sessionId}")
    resp.cookie("authCode", auth.code, 3600 * 24 * 30) // 30 days
    resp.cookie("sessionId", auth.sessionId) // session cookie - no expiry
    resp.redirect(s"http://localhost:$port/auth?user=${encode(auth.userId)}&since=${encode(since.toString)}&session=${encode(sessionId)}&auth=${auth.code}")
    NodeSeq.Empty
  }
}

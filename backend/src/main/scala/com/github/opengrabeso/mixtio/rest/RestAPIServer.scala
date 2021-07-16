package com.github.opengrabeso.mixtio
package rest

import com.github.opengrabeso.mixtio.Main.StravaAuthResult
import com.github.opengrabeso.mixtio.requests.Cleanup
import com.github.opengrabeso.mixtio.requests.Cleanup.BackgroundCleanup

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import common.model._
import io.udash.rest.raw.HttpErrorException

import scala.collection.mutable

object RestAPIServer extends RestAPI with RestAPIUtils {

  def identity(in: String) = {
    syncResponse(in)
  }

  private def sessionFileName(session: String, userId: String, file: String) = {
    Storage.FullName(Main.namespace.session(session), file, userId)
  }
  /**
    * @return session id
    */
  def createUser(auth: StravaAuthResult): StravaAuthResult = {
    Storage.store(sessionFileName(auth.sessionId, auth.userId, "auth"), auth)
    println(s"createUser ${auth.userId}, session ${auth.sessionId}")
    auth
  }

  def getAuth(userId: String, authToken: String, session: String) = {
    val logging = false
    if (logging) println(s"Try auth for user $userId, session $session")
    val auth = Storage.load[StravaAuthResult](sessionFileName(session, userId, "auth"))
    auth.map { a =>
      if (a.token == authToken) {
        if (logging) println(s"Get userAPI for user $userId, session $session, auth.session ${a.sessionId}")
        a
      } else {
        if (logging) println("Provided auth token does not match")
        throw HttpErrorException(401, s"Provided auth token '$authToken' does not match the one stored on the server")
      }
    }.getOrElse {
      if (logging) println("User ID not authenticated")
      throw HttpErrorException(401, "User ID not authenticated. Page reload may be necessary.")
    }
  }
  def userAPI(userId: String, authToken: String, session: String): UserRestAPI = {
    val auth = getAuth(userId, authToken, session)
    new UserRestAPIServer(auth)
  }

  def strava(userId: String, authToken: String, session: String): StravaRestAPI = {
    val auth = getAuth(userId, authToken, session)
    new StravaRestAPIServer(auth, session)
  }

  def now = syncResponse {
    ZonedDateTime.now()
  }

  def cleanup(pars: String) = syncResponse {
    Cleanup.BackgroundCleanup.execute(pars)
  }

}

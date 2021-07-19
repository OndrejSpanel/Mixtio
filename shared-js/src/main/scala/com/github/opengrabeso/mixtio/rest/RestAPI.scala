package com.github.opengrabeso.mixtio
package rest

import java.time.ZonedDateTime

import io.udash.rest._

import scala.concurrent.Future

trait RestAPI {

  @GET
  def identity(@Path in: String): Future[String]

  /* Caution: cookie parameters are used from the dom.document when called from Scala.js */
  @Prefix("user")
  def userAPI(@Path userId: String, @Cookie authToken: String, @Cookie sessionId: String): UserRestAPI

  // avoid using cookie parameters, we need to use this from tasks
  def strava(userId: String, authToken: String, session: String): StravaRestAPI

  @GET
  def now: Future[ZonedDateTime]

  def cleanup(pars: String): Future[Unit]
}

object RestAPI extends RestApiCompanion[EnhancedRestImplicits,RestAPI](EnhancedRestImplicits) {
  final val apiVersion = "2.1-beta"
}

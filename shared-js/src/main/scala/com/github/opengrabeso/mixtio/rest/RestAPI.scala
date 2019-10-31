package com.github.opengrabeso.mixtio
package rest

import java.time.ZonedDateTime

import com.github.opengrabeso.mixtio.common.model.BinaryData
import io.udash.rest._

import scala.concurrent.Future

trait RestAPI {
  @GET
  def identity(@Path in: String): Future[String]

  @Prefix("user")
  def userAPI(@Path userId: String, @Cookie authCode: String, @Cookie sessionId: String): UserRestAPI

  @GET
  def now: Future[ZonedDateTime]

  // create a limited session (no Strava access) - used for push uploader
  def limitedSession(userId: String, authCode: String): Future[String]
}

object RestAPI extends RestApiCompanion[EnhancedRestImplicits,RestAPI](EnhancedRestImplicits)

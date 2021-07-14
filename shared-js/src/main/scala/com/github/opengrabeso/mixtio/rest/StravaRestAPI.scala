package com.github.opengrabeso.mixtio
package rest

import io.udash.rest._

import scala.concurrent.Future

trait StravaRestAPI {
  def uploadFile(@Path key: String): Future[Unit]
  def waitForUpload(@Path key: String, @Path id: Long): Future[Unit]
}


object StravaRestAPI extends RestApiCompanion[EnhancedRestImplicits,StravaRestAPI](EnhancedRestImplicits)
package com.github.opengrabeso.mixtio
package rest

import io.udash.rest._

import scala.concurrent.Future

trait StravaRestAPI {
  @POST // used as Cloud task, a single parameter named pars is needed
  def uploadFile(pars: String): Future[Unit]

  @POST // used as Cloud task, a single parameter named pars is needed
  def waitForUpload(pars: (String, Long)): Future[Unit]
}


object StravaRestAPI extends RestApiCompanion[EnhancedRestImplicits,StravaRestAPI](EnhancedRestImplicits)
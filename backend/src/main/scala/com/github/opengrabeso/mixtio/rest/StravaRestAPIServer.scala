package com.github.opengrabeso.mixtio
package rest

import requests.{UploadResultToStrava, WaitForStravaUpload}

class StravaRestAPIServer(val auth: Main.StravaAuthResult, val sessionId: String) extends StravaRestAPI with RestAPIUtils {

  def uploadFile(pars: String) = syncResponse {
    UploadResultToStrava(auth, sessionId).execute(pars)
  }

  def waitForUpload(pars: (String, Long)) = syncResponse {
    // check if the upload has finished
    // Strava recommends polling no more than once a second
    WaitForStravaUpload(auth, sessionId).execute(pars)
  }

}

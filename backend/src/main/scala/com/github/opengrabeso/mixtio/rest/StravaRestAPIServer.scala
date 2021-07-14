package com.github.opengrabeso.mixtio
package rest

import requests.{UploadResultToStrava, WaitForStravaUpload}

class StravaRestAPIServer(val auth: Main.StravaAuthResult, val sessionId: String) extends StravaRestAPI with RestAPIUtils {

  def uploadFile(key: String) = syncResponse {
    UploadResultToStrava(auth, sessionId).execute(key)
  }

  def waitForUpload(key: String, id: Long) = syncResponse {
    // check if the upload has finished
    // Strava recommends polling no more than once a second
    WaitForStravaUpload(auth, sessionId).execute((key, id))
  }

}

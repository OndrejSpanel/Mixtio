package com.github.opengrabeso.mixtio
package requests

import Main._

import common.model._

import Optimize._
trait ActivityStorage {

  def storeActivity(stage: String, act: ActivityEvents, userId: String) = {
    Storage.store(stage, act.id.id.filename, userId, act.header, act, Seq("digest" -> act.id.digest), Seq("startTime" -> act.id.startTime.toString))
  }

  def upload(activity: ActivityEvents)(auth: StravaAuthResult, sessionId: String): String = {
    val uploadFiltered = activity.applyUploadFilters(auth)
    // export here, or in the worker? Both is possible

    // filename is not strong enough guarantee of uniqueness, timestamp should be (in single user namespace)
    val uniqueName = uploadFiltered.id.id.filename + "_" + System.currentTimeMillis().toString
    // are any metadata needed?
    Storage.store(namespace.upload(sessionId), uniqueName, auth.userId, uploadFiltered.header, uploadFiltered)

    BackgroundTasks.addTask(UploadResultToStrava(auth, sessionId), uniqueName, System.currentTimeMillis())

    val uploadResultNamespace = Main.namespace.uploadResult(sessionId)
    val uploadId = Storage.FullName(uploadResultNamespace, uniqueName, auth.userId).name
    println(s"Queued task $uniqueName with uploadId=$uploadId")
    uploadId

  }

  def uploadMultiple(merged: Seq[ActivityEvents])(auth: StravaAuthResult, sessionId: String): Seq[String] = {
    // store everything into a session storage, and make background tasks to upload it to Strava

    for (activity <- merged) yield {
      upload(activity)(auth, sessionId)
    }
  }

}

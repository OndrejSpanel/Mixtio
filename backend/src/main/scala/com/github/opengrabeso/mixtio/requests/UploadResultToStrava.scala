package com.github.opengrabeso.mixtio
package requests

import com.google.appengine.api.taskqueue._

import scala.xml.NodeSeq

sealed trait UploadStatus {
  def xml: NodeSeq
  override def toString: String = xml.toString
}

@SerialVersionUID(10)
case class UploadInProgress(uploadId: Long) extends UploadStatus {
  def xml = Nil
}

@SerialVersionUID(10)
case class UploadDone(stravaId: Long) extends UploadStatus {
  def xml = <done>{stravaId}</done>
}

@SerialVersionUID(10)
case class UploadDuplicate(dupeId: Long) extends UploadStatus {
  def xml = <duplicate>{dupeId}</duplicate>
}

@SerialVersionUID(10)
case class UploadError(ex: Throwable) extends UploadStatus {
  def xml = <error>{ex.getMessage}</error>

}
// background push queue task

@SerialVersionUID(10L)
case class UploadResultToStrava(auth: Main.StravaAuthResult, sessionId: String) extends BackgroundTasks.TaskDescription[String] {

  def execute(key: String)= {
    val api = new strava.StravaAPI(auth.token)

    val uploadNamespace = Main.namespace.upload(sessionId)
    val uploadResultNamespace = Main.namespace.uploadResult(sessionId)

    for (upload <- Storage.load2nd[ActivityEvents](Storage.getFullName(uploadNamespace, key, auth.userId))) {

      val export = FitExport.export(upload)

      val ret = api.uploadRawFileGz(export, "fit.gz")

      Storage.store(Storage.FullName(uploadResultNamespace, key, auth.userId), ret)
      ret match {
        case UploadInProgress(uploadId) =>
          println(s"Upload started $uploadId")
          Storage.store(Storage.FullName(uploadResultNamespace, key, auth.userId), UploadInProgress(uploadId))
          val eta = System.currentTimeMillis() + 1000
          BackgroundTasks.addTask(WaitForStravaUpload(auth, sessionId), (key, uploadId), eta)
        case done =>
          Storage.store(Storage.FullName(uploadResultNamespace, key, auth.userId), done)
      }
      Storage.delete(Storage.FullName(uploadNamespace, key, auth.userId))
    }
  }

  override def path = s"/rest/user/${auth.userId}/strava/{$sessionId}/uploadFile"

}

case class WaitForStravaUpload(auth: Main.StravaAuthResult, sessionId: String) extends BackgroundTasks.TaskDescription[(String, Long)] {

  def execute(pars: (String, Long)) = {
    val (key, id) = pars
    // check if the upload has finished
    val api = new strava.StravaAPI(auth.token)
    val uploadResultNamespace = Main.namespace.uploadResult(sessionId)
    val done = api.activityIdFromUploadId(id)
    done match {
      case UploadInProgress(_) =>
        // still processing - retry
        val now = System.currentTimeMillis()
        BackgroundTasks.addTask(this, (key, id), now + 2000)
      case _ =>
        Storage.store(Storage.FullName(uploadResultNamespace, key, auth.userId), done)

    }
  }

  override def path = s"/rest/user/${auth.userId}/strava/{$sessionId}/waitForUpload"
}
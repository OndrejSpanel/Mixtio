package com.github.opengrabeso.mixtio
package rest

import java.io.ByteArrayInputStream
import java.time.{ZoneId, ZonedDateTime}

import Main.namespace
import Optimize._
import requests.Upload.storeFromStream
import requests.{UploadDone, UploadDuplicate, UploadError, UploadInProgress}
import com.google.api.client.http.HttpResponseException
import shared.Timing
import common.model._
import io.udash.rest.raw.{HttpBody, HttpErrorException}
import org.apache.commons.fileupload.{FileItemStream, RequestContext}
import org.apache.commons.fileupload.disk.DiskFileItemFactory
import org.apache.commons.fileupload.servlet.ServletFileUpload
import org.apache.commons.io.IOUtils

class UserRestAPIServer(val userAuth: Main.StravaAuthResult) extends UserRestAPI with RestAPIUtils with requests.ActivityStorage {
  def name = syncResponse {
    userAuth.name
  }
  def settings: UserRestSettingsAPI = new UserRestSettingsAPIServer(userAuth.userId)

  def allSettings = syncResponse {
    Settings(userAuth.userId)
  }


  def logout = syncResponse {
    // TODO: delete all user info - use non-REST API
  }

  def lastStravaActivities(count: Int) = syncResponse {
    val timing = Timing.start()
    val uri = "https://www.strava.com/api/v3/athlete/activities"
    val request = RequestUtils.buildGetRequest(uri, userAuth.token, s"per_page=$count")

    val ret = Main.parseStravaActivities(request.execute().getContent)
    timing.logTime(s"lastStravaActivities ($count)")
    ret
  }

  def stagedActivities(notBefore: ZonedDateTime) = syncResponse {
    Main.stagedActivities(userAuth, notBefore)
  }

  def deleteActivities(ids: Seq[FileId]) = syncResponse {
    for (id <- ids) {
      Storage.delete(Storage.getFullName(Main.namespace.stage, id.filename, userAuth.userId))
      println(s"Delete ${Main.namespace.stage} ${id.filename} ${userAuth.userId}")
    }
  }

  def importFromStrava(stravaNumId: Long) = syncResponse {
    try {
      val activityData = Main.getEventsFrom(userAuth.token, stravaNumId.toString)
      com.github.opengrabeso.mixtio.requests.Process.storeActivity(Main.namespace.stage, activityData, userAuth.userId)
      activityData.id
    } catch {
      case ex: HttpResponseException if ex.getStatusCode == 404 =>
        throw HttpErrorException(400, "Activity data not found", ex.getCause)

    }
  }

  def processOne[T](id: FileId, events: Seq[(String, Int)], time: Int)(process: (Int, ActivityEvents) => T): Option[T] = {
    Storage.load2nd[ActivityEvents](Storage.getFullName(Main.namespace.edit, id.filename, userAuth.userId)).flatMap { activity =>
      val editedEvents = events.collect {
        case (ei, time) if (ei.startsWith("split")) =>
          val sportName = ei.substring("split".length)
          SplitEvent(activity.timeInActivity(time), Event.Sport.withName(sportName))
        case ("delete", time)  =>
          // Sport does not matter, will be deleted, but we need a corresponding split event so that split and span functions work
          SplitEvent(activity.timeInActivity(time), Event.Sport.Workout)
        case ("lap", time) =>
          LapEvent(activity.timeInActivity(time))
        case (_, time) =>
          PauseEvent(0, activity.timeInActivity(time)) // it does not matter what event we created, as long as it is not lap or split
        // we ignore empty events?
      } :+ EndEvent(activity.endTime) // TODO: EndEvent could probably be removed completely?

      val activityToDeleteFrom = activity.copy(events = editedEvents.toArray)

      val secondsToDelete = (events zip events.drop(1)).collect { case (("delete", beg), (_, end)) =>
        (beg, end)
      }
      val intervalsToDelete = secondsToDelete.map { case (beg, end) =>
        (activity.timeInActivity(beg), activity.timeInActivity(end))
      }

      // first remove any disabled intervals
      val activityWithIntervalsDeleted = intervalsToDelete.foldLeft[Option[ActivityEvents]](Some(activityToDeleteFrom)) {
        case (None, _) =>
          None
        case (Some(activity), (beg, end)) =>
          import common.Util._
          assert( beg <= end)
          if (end >= activity.endTime && beg <= activity.startTime) {
            None
          } else if (activity.startTime <= end && activity.endTime >= beg) {
            // some overlap, we need to delete the beg..end part, i.e. keep (activity.startTime .. beg) and (end .. activity.endTime)
            val logging = false
            if (logging) println(s"Delete ($beg..$end) from $activity")
            val keepBeforeBeg = activity.span(beg)._1
            val keepAfterEnd = activity.span(end)._2
            (keepBeforeBeg, keepAfterEnd) match {
              case (Some(a1), Some(a2)) =>
                if (logging) println(s"take $a1 + $a2")
                Some(a1.merge(a2))
              case (Some(a), None) =>
                if (logging) println(s"take $a")
                Some(a)
              case (None, Some(a)) =>
                if (logging) println(s"take $a")
                Some(a)
              case (None, None) =>
                None
            }
          } else {
            Some(activity)
          }
      }

      // now select the interval we want to process
      for {
        a <- activityWithIntervalsDeleted
        split <- a.split(activity.timeInActivity(time))
      } yield {
        process(time, split)
      }
    }
  }

  def downloadEditedActivity(id: FileId, sessionId: String, events: Seq[(String, Int)], time: Int) = syncResponse {
    val data = processOne(id, events, time) { (_, up) =>
      FitExport.export(up)
    }
    BinaryData(data.get)
  }

  def sendEditedActivityToStrava(id: FileId, sessionId: String, events: Seq[(String, Int)], time: Int) = syncResponse {
    val uploadIds = processOne(id, events, time) { (_, up) =>
      upload(up)(userAuth, sessionId)
    }
    uploadIds
  }

  def pollUploadResults(uploadIds: Seq[String], sessionId: String) = syncResponse {
    val uploadResultNamespace = Main.namespace.uploadResult(sessionId)
    val resultsFiles = Storage.enumerate(uploadResultNamespace, userAuth.userId)

    val results = resultsFiles.flatMap { case (uploadId, resultFilename) =>
      val load = Storage.load[requests.UploadStatus](Storage.FullName(uploadResultNamespace, resultFilename, userAuth.userId))
      load.map { x =>
        val (ended, ret) = x match {
          case UploadInProgress(_) =>
            false -> UploadProgress.Pending(uploadId.name)
          case UploadDone(stravaId) =>
            true -> UploadProgress.Done(stravaId, uploadId.name)
          case UploadError(ex) =>
            true -> UploadProgress.Error(uploadId.name, ex.getLocalizedMessage)
          case UploadDuplicate(dupeId) =>
            true -> UploadProgress.Duplicate(uploadId.name, dupeId) // Strava no longer seems to return specific error for duplicates
        }
        if (ended) {
          // once reported, delete it
          println(s"Upload ${uploadId.name} of $resultFilename completed")
          Storage.delete(Storage.FullName(uploadResultNamespace, resultFilename, userAuth.userId))
        }
        ret
      }
    }.toSeq
    results
  }

  def mergeActivitiesToEdit(ops: Seq[FileId], sessionId: String) = syncResponse {
    val toMerge = ops.flatMap { op =>
      Storage.load[ActivityHeader, ActivityEvents](Storage.getFullName(namespace.stage, op.filename, userAuth.userId)).map(_._2.applyFilters(userAuth))
    }

    if (toMerge.nonEmpty) {
      // first merge all GPS data
      // then merge in all attribute data
      val (toMergeGPS, toMergeAttrRaw) = toMerge.partition(_.hasGPS)
      val timeOffset = Settings(userAuth.userId).questTimeOffset
      val toMergeAttr = toMergeAttrRaw.map(_.timeOffset(-timeOffset))

      val merged = if (toMergeGPS.nonEmpty) {
        val gpsMerged = toMergeGPS.reduceLeft(_ merge _)
        (gpsMerged +: toMergeAttr).reduceLeft(_ merge _)
      } else {
        toMerge.reduceLeft(_ merge _)
      }

      // we cannot perform and pause processing when there are no GPS data
      val prepare = if (toMergeGPS.nonEmpty) merged.cleanPositionErrors.processPausesAndEvents else merged
      // TODO: make sure edited name is unique
      Storage.store(namespace.edit, prepare.id.id.filename, userAuth.userId, prepare.header, prepare)

      val events = prepare.events.map { e =>
        e -> merged.distanceForTime(e.stamp)
      }

      Some((prepare.id.id, events))

    } else {
      None
    }
  }

  def routeData(id: FileId) = syncResponse {
    // TODO: consider some activity caching on the frontend/backend side
    Storage.load2nd[ActivityEvents](Storage.getFullName(Main.namespace.edit, id.filename, userAuth.userId))
      .map(_.routeData).getOrElse(Nil)

  }

  def upload(files: HttpBody) = syncResponse {

    files match {
      case body: HttpBody.NonEmpty =>
        val fif = new DiskFileItemFactory()
        val maxMB = 32
        fif.setSizeThreshold(maxMB * 1024 * 1024)

        val upload = new ServletFileUpload(fif)

        object simulatedServletContext extends RequestContext {
          def getCharacterEncoding = "utf-8" // is this correct?
          def getContentType = body.contentType
          def getContentLength = body.bytes.length
          def getInputStream = new ByteArrayInputStream(body.bytes)
        }
        val items = upload.getItemIterator(simulatedServletContext)

        val itemsIterator = new Iterator[FileItemStream] {
          def hasNext = items.hasNext

          def next() = items.next
        }

        val builder = Seq.newBuilder[ActivityHeader]
        // TODO: obtain client timezone - neeeded when uploading Quest XML files
        var timezone = Option.empty[String]
        itemsIterator.foreach { item =>
          if (item.isFormField && item.getFieldName == "timezone") {
            timezone = Some(IOUtils.toString(item.openStream(), "UTF-8"))
          } else if (!item.isFormField && item.getFieldName == "files") {
            if (item.getName != "") {
              val useTimezone = timezone.getOrElse {
                println("Warning: timezone not provided in the request, using system default instead")
                ZoneId.systemDefault().toString
              }
              builder ++= storeFromStream(userAuth.userId, item.getName, useTimezone, item.openStream()).map { e =>
                ActivityHeader(e.id, e.hasGPS, e.hasAttributes, e.computeSpeedStats)
              }
            }
          }
        }
        builder.result()
      case _ =>
        throw HttpErrorException(400)
    }

  }
}

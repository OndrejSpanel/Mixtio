package com.github.opengrabeso.mixtio
package requests

import java.time.ZonedDateTime
import Main._
import common.Util._
import common.model._

/**
  * User specific cleanup, requires user access tokens for Strava */

@SerialVersionUID(10L)
case class UserCleanup(auth: StravaAuthResult, before: ZonedDateTime) {
  // TODO: implement properly (using cron?)
  def run(): Unit = {

    val d = Storage.enumerate(namespace.stage, auth.userId)

    // clean activities before "before", as those are never listed to the user
    // verify they are already stored on Strava, if not, keep then until a global expiry cleanup will handle them
    val headers = d.flatMap { a =>
      Storage.load[ActivityHeader](a._1).map(a -> _)
    }

    val oldStaged = headers.filter(_._2.id.startTime < before).toSeq.sortBy(_._2.id.startTime)

    // TODO: check and cleanup for activities older than the history
    val (_, oldStravaActivities) = Main.recentStravaActivitiesHistory(auth, 2)

    val headersToClean = oldStaged.flatMap(h => oldStravaActivities.find(_ isMatching h._2.id).map(h -> _))

    if (headersToClean.isEmpty && oldStaged.nonEmpty && oldStravaActivities.nonEmpty) {
      // nothing to clean, perform one more request to get older Strava data if needed
      val oldestStaged = oldStaged.map(_._2.id.startTime).min
      val oldestStrava = oldStravaActivities.map(_.startTime).min
      if (oldestStrava > oldestStaged) { // some older Strava data may be available to match
        val (_, olderStravaActivities) = Main.recentStravaActivitiesHistory(auth, 4)

        val olderHeadersToClean = oldStaged.flatMap(h => olderStravaActivities.find(_ isMatching h._2.id).map(h -> _))
        cleanFiles(olderHeadersToClean)
      }
    } else {
      cleanFiles(headersToClean)
    }
  }

  def cleanFiles(headersToClean: Seq[(((Storage.FullName, String), ActivityHeader), ActivityId)]) = {
    for ((((file,_), h), onStrava) <- headersToClean) {
      println(s"Cleaning stage file $onStrava ${h.id} $file")
      Storage.delete(file)
    }

  }

}

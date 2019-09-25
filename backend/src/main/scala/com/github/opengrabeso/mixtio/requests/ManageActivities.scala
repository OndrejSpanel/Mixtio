package com.github.opengrabeso.mixtio
package requests

import java.time.ZonedDateTime
import Main._

object ManageActivities extends SelectActivity("/selectActivity") {
  override def title = "select activities to process"

  override def sources(before: ZonedDateTime) = {
    <div>
      <h2>Data sources</h2>
      <a href="getFiles">Upload files...</a>
      <a href="staging">Staging...</a>
      <a href="settings">Settings...</a>
      <hr/>
    </div>
  }

  override def filterListed(activity: ActivityHeader, strava: Option[ActivityId]) = strava.isEmpty

  override def ignoreBefore(stravaActivities: Seq[ActivityId]) = defaultIgnoreBefore(stravaActivities)

}

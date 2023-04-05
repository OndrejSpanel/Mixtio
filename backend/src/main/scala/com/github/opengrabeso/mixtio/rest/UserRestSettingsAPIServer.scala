package com.github.opengrabeso.mixtio
package rest

import scala.concurrent.Future

class UserRestSettingsAPIServer(userId: String) extends UserRestSettingsAPI with RestAPIUtils {
  private def getSetting[T](f: SettingsStorage => T) = syncResponse {
    f(Settings(userId))
  }
  private def setSetting(f: SettingsStorage => SettingsStorage): Future[Unit] = syncResponse {
    val original = Settings(userId)
    val changed = f(original)
    Settings.store(userId, changed)

  }

  def quest_time_offset = getSetting(_.questTimeOffset)
  def max_hr = getSetting(_.maxHR)
  def elev_filter = getSetting(_.elevFilter)

  def quest_time_offset(v: Int) = setSetting(_.setQuestTimeOffset(Some(v)))
  def max_hr(v: Int) = setSetting(_.setMaxHR(Some(v)))
  def elev_filter(v: Int) = setSetting(_.setElevFilter(Some(v)))
}

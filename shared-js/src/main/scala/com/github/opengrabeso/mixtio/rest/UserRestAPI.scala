package com.github.opengrabeso.mixtio
package rest

import java.time.{Instant, ZoneId, ZoneOffset, ZonedDateTime}

import com.avsystem.commons.serialization.whenAbsent
import common.model._
import io.udash.rest._
import io.udash.rest.raw.HttpBody

import scala.concurrent.Future

trait UserRestAPI {
  def logout: Future[Unit]

  def settings: UserRestSettingsAPI

  @GET("settings")
  def allSettings: Future[SettingsStorage]

  @GET
  def name: Future[String]

  @GET
  def lastStravaActivities(@whenAbsent(15) count: Int): Future[Seq[ActivityHeader]]

  @GET
  def stagedActivities(@Query @whenAbsent(Instant.ofEpochMilli(0).atZone(ZoneOffset.UTC)) notBefore: ZonedDateTime): Future[Seq[ActivityHeader]]

  def deleteActivities(ids: Seq[FileId]): Future[Unit]

  def importFromStrava(stravaId: Long): Future[ActivityId]

  def pollUploadResults(uploadIds: Seq[String], sessionId: String): Future[Seq[UploadProgress]]

  /// this will create one activity in the "edit" namespace
  def mergeActivitiesToEdit(ids: Seq[FileId], sessionId: String): Future[Option[(FileId, Seq[(Event, Double)])]]

  /// return GeoJSON directly usable for mapboxgl
  def routeData(id: FileId): Future[Seq[(Double, Double, Double, Double)]]

  def sendEditedActivityToStrava(id: FileId, sessionId: String, events: Seq[(String, Int)], time: Int): Future[Option[String]]

  def downloadEditedActivity(id: FileId, sessionId: String, events: Seq[(String, Int)], time: Int): Future[BinaryData]

  // upload a file to Mixtio
  @CustomBody def upload(files: HttpBody): Future[Seq[ActivityHeader]]
}

object UserRestAPI extends RestApiCompanion[EnhancedRestImplicits,UserRestAPI](EnhancedRestImplicits)
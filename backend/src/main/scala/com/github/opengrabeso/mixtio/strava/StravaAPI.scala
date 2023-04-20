package com.github.opengrabeso.mixtio
package strava

import java.io._
import java.time.ZonedDateTime
import java.util.zip.GZIPOutputStream

import com.fasterxml.jackson.databind.JsonNode
import com.google.api.client.http._
import common.Util._
import requests._

import scala.util.{Failure, Success, Try}

object StravaAPI {
  val localTest = false

  val stravaSite = "www.strava.com"
  val stravaRootURL = "/api/v3/"
  val stravaRoot = "https://" + stravaSite + stravaRootURL

  def buildURI(path: String): String = {
    if (!localTest) stravaRoot + path
    else "http://localhost/JavaEE-war/webresources/generic/"
  }

  def buildPostData(params: (String, String)*) = {
    params.map(p => s"${p._1}=${p._2}").mkString("&")
  }
}

class StravaAPI(authString: String) {

  import RequestUtils._
  import StravaAPI._

  // see https://strava.github.io/api/

  def athlete: String = {
    val request = buildGetRequest(buildURI("athlete"), authString, "")

    val result = request.execute().parseAsString
    result
  }

  def mostRecentActivityTime: Option[ZonedDateTime] = {
    // we might want to add parameters page=0, per_page=1
    val request = buildGetRequest(buildURI("athlete/activities"), authString, "")

    val result = request.execute().getContent

    val json = jsonMapper.readTree(result)

    val times = (0 until json.size).flatMap { i =>
      val start_date = Option(json.get(i).path("start_date").textValue)
      start_date match {
        case Some(timeString) =>
          val time = Try {
            ZonedDateTime.parse(timeString)
          }
          time.toOption
        case _ =>
          Nil
      }
    }

    val mostRecentTime = if (times.nonEmpty) Some(times.max) else None

    mostRecentTime
  }

  def deleteActivity(id: Long): Unit = {
    val request = buildDeleteRequest(buildURI(s"activities/$id"), authString, "")

    request.execute()
  }

  def uploadRawFileGz(moveBytesOriginal: Array[Byte], fileType: String): UploadStatus = {

    val baos = new ByteArrayOutputStream()
    val zipStream = new GZIPOutputStream(baos)
    try {
      zipStream.write(moveBytesOriginal)
    } finally {
      zipStream.close()
    }
    uploadRawFile(baos.toByteArray, fileType)
  }

  def statusFromUploadJson(resultJson: JsonNode): UploadStatus = {
    val DupeRegex = ".* duplicate of activity (\\d+).*".r

    val id = resultJson.path("id").longValue()
    (
      Option(resultJson.path("status").textValue),
      Option(resultJson.path("activity_id").numberValue),
      Option(resultJson.path("error").textValue)
    ) match {
      case (Some(status), _, _) if status == "Your activity is still being processed." =>
        UploadInProgress(id)
      case (Some(_), _, Some(DupeRegex(dupeId))) =>
        UploadDuplicate(dupeId.toLong)
      case (_, Some(actId), _) if actId.longValue != 0 =>
        UploadDone(actId.longValue)
      case (Some(status), _, _)  =>
        UploadError(new UnsupportedOperationException(status))
      case _ =>
        UploadError(new UnsupportedOperationException)
    }
  }

  def activityIdFromUploadId(id: Long): UploadStatus = {
    try {
      val request = buildGetRequest(buildURI(s"uploads/$id"), authString, "")
      //request.getHeaders.set("Expect",Array("100-continue"))
      request.getHeaders.setAccept("*/*")

      val response = request.execute()
      val resultJson = jsonMapper.readTree(response.getContent)
      statusFromUploadJson(resultJson)
    } catch {
      case ex: HttpResponseException if ex.getStatusCode == 404 =>
        UploadError(ex)
      case ex: Exception =>
        ex.printStackTrace()
        UploadError(ex)
    }

  }

  def uploadRawFile(sendBytes: Array[Byte], fileType: String): UploadStatus = {

    Try {
      // see https://strava.github.io/api/v3/uploads/ -
      val body = new MultipartContent() // default is "multipart/related"
      body.getMediaType.setSubType("form-data") // use the current type so that it contains a "boundary"
      //body.setMediaType(new HttpMediaType("multipart", "form-data"))

      def textPart(name: String, value: String) = {
        new MultipartContent.Part(
          new HttpHeaders().set("Content-Disposition", s"""form-data; name="$name""""),
          ByteArrayContent.fromString("text/plain", value)
        )
      }
      def binaryPart(name: String, filename: String, bytes: Array[Byte]) = {
        new MultipartContent.Part(
          new HttpHeaders().set("Content-Disposition", s"""form-data; name="$name"; filename="$filename""""),
          new ByteArrayContent("application/octet-stream", bytes)
        )
      }

      body.addPart(binaryPart("file", "file." + fileType, sendBytes))
      body.addPart(textPart("data_type", fileType))
      //body.addPart(textPart("private", "1"))


      //def buildURI(x: String) = "http://localhost:3000/multipart/singlefileupload"
      //def buildURI(x: String) = "https://ptsv2.com/t/39e9v-1581500835/post"
      //def buildURI(x: String) = "https://enl1aic4z66k.x.pipedream.net"
      //def authString = "Bearer xxxxxxx"

      val request = buildPostRequest(buildURI("uploads"), authString, "", body)
      request.getHeaders.set("Expect", Array("100-continue"))
      request.getHeaders.setAccept("*/*")

      val response = request.execute()
      val resultString = response.getContent

      // we expect to receive 201

      val json = jsonMapper.readTree(resultString)

      statusFromUploadJson(json)
    } .fold(UploadError, identity)
  }
}

package com.github.opengrabeso.mixtio

import com.fasterxml.jackson.databind.ObjectMapper
import com.google.api.client.http.{GenericUrl, HttpContent, HttpRequest, HttpRequestInitializer}
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.JsonObjectParser
import com.google.api.client.json.gson.GsonFactory

object RequestUtils {
  private val transport = new NetHttpTransport()
  val jsonFactory = new GsonFactory()
  val jsonMapper = new ObjectMapper()

  val requestFactory = transport.createRequestFactory(new HttpRequestInitializer() {
    override def initialize(request: HttpRequest) = request.setParser(new JsonObjectParser(jsonFactory))
  })

  def buildPostRequest(uri: String, authToken: String, parameters: String, content: HttpContent): HttpRequest = {
    val request = requestFactory.buildPostRequest(new GenericUrl(uri + "?" + parameters), content)
    authorizeRequest(request, authToken)
  }

  def authorizeRequest(request: HttpRequest, authToken: String) = {
    val headers = request.getHeaders
    headers.put("authorization", s"Bearer $authToken")
    request
  }

  def buildGetRequest(uri: String, authToken: String, parameters: String): HttpRequest = {
    val request = requestFactory.buildGetRequest(new GenericUrl(uri + "?" + parameters))

    authorizeRequest(request, authToken)
  }

  def buildDeleteRequest(uri: String, authToken: String, parameters: String): HttpRequest = {
    val request = requestFactory.buildDeleteRequest(new GenericUrl(uri + "?" + parameters))
    authorizeRequest(request, authToken)
  }

  def buildGetRequest(uri: String, parameters: Map[String, String] = Map.empty): HttpRequest = {
    val parametersAsString = parameters.map(kv => kv._1 + "=" + kv._2).mkString("&")
    val request = requestFactory.buildGetRequest(new GenericUrl(uri + "?" + parametersAsString))
    request
  }


}

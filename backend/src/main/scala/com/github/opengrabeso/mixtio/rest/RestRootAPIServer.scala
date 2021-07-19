package com.github.opengrabeso.mixtio.rest

// this class exists only so that all rest APIs are prefixed with /rest

object RestRootAPIServer extends RestRootAPI {
  override def rest = RestAPIServer
}

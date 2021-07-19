package com.github.opengrabeso.mixtio
package rest

import io.udash.rest._

trait RestRootAPI {
  def rest: RestAPI
}

object RestRootAPI extends RestApiCompanion[EnhancedRestImplicits,RestRootAPI](EnhancedRestImplicits)


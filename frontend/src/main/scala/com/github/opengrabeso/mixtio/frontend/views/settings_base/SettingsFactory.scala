package com.github.opengrabeso.mixtio
package frontend
package views
package settings_base

import io.udash._

trait SettingsFactory extends PageFactoryUtils {
  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  def loadSettings(model: ModelProperty[SettingsModel], userService: services.UserContextService): Unit = {
    model.subProp(_.settings.questTimeOffset) //.addValidator(new NumericRangeValidator(-120, +120))
    model.subProp(_.settings.maxHR) //.addValidator(new NumericRangeValidator(90, 240))

    for (userSettings <- userService.api.get.allSettings) {
      model.subProp(_.settings).set(userSettings)
      model.subProp(_.loading).set(false)
    }

  }

}

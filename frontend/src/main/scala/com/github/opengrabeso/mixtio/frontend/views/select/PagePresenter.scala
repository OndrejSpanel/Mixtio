package com.github.opengrabeso.mixtio
package frontend
package views.select

import java.time.ZonedDateTime

import common.model._
import common.Util._
import common.ActivityTime._
import routing._
import io.udash._

import scala.concurrent.ExecutionContext

/** Contains the business logic of this view. */
class PagePresenter(
  model: ModelProperty[PageModel],
  application: Application[RoutingState],
  userService: services.UserContextService
)(implicit ec: ExecutionContext) extends Presenter[SelectPageState.type] {

  model.subProp(_.showOnlyRecent).listen { p =>
    loadActivities(p)
  }

  def loadActivities(onlyRecent: Boolean) = {
    def findMatchingStrava(ids: Seq[ActivityHeader], strava: Seq[ActivityId]): Seq[(ActivityHeader, Option[ActivityId])] = {
      ids.map( a => a -> strava.find(_ isMatching a.id))
    }

    for (userAPI <- userService.api) {

      val normalCount = 15
      def filterListed(activity: ActivityHeader, strava: Option[ActivityId]) = true

      userAPI.lastStravaActivities((normalCount * 2).toInt).map { allActivities =>

        val (stravaActivities, oldStravaActivities) = allActivities.splitAt(normalCount)

        val showOnlyRecent = false
        val notBefore = if (showOnlyRecent) ZonedDateTime.now() minusMonths 24
        else stravaActivities.map(a => a.startTime).min

        userAPI.stagedActivities(notBefore).foreach { stagedActivities =>

          val neverBefore = alwaysIgnoreBefore(stravaActivities)

          // never display any activity which should be cleaned by UserCleanup
          val oldStagedActivities = stagedActivities.filter(_.id.startTime < neverBefore)
          val toCleanup = findMatchingStrava(oldStagedActivities, oldStravaActivities).flatMap { case (k,v) => v.map(k -> _)}
          val recentActivities = (stagedActivities diff toCleanup.map(_._1)).filter(_.id.startTime >= notBefore).sortBy(_.id.startTime)
          val mostRecentStrava = stravaActivities.headOption.map(_.startTime)

          val recentToStrava = findMatchingStrava(recentActivities, stravaActivities ++ oldStravaActivities).filter((filterListed _).tupled)

          model.subProp(_.activities).set(recentToStrava.map { case (act, actStrava) =>
            val ignored = actStrava.isDefined || mostRecentStrava.exists(_ >= act.id.startTime)
            ActivityRow(act, actStrava, !ignored)
          })
          model.subProp(_.loading).set(false)
        }
      }.failed.foreach { ex =>
        println(s"Failed $ex")
        model.subProp(_.loading).set(false)
      }
    }


  }

  override def handleState(state: SelectPageState.type): Unit = {
  }

  def gotoDummy(): Unit = {
    application.goTo(DummyPageState)
  }
  def gotoSettings(): Unit = {
    application.goTo(SettingsPageState)
  }
}

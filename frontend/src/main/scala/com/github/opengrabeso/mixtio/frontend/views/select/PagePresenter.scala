package com.github.opengrabeso.mixtio
package frontend
package views
package select

import java.time.{ZoneOffset, ZonedDateTime}

import common.model._
import common.Util._
import common.ActivityTime._
import routing._
import io.udash._

import scala.concurrent.{ExecutionContext, Future, Promise}
import services.UserContextService

import scala.scalajs.js
import scala.util.{Failure, Success}

/** Contains the business logic of this view. */
class PagePresenter(
  model: ModelProperty[PageModel],
  application: Application[RoutingState],
  userService: services.UserContextService
)(implicit ec: ExecutionContext) extends Presenter[SelectPageState.type] {

  model.subProp(_.showAll).listen { p =>
    loadActivities(p)
  }

  def loadActivities(showAll: Boolean) = {
    val load = userService.loadCached(showAll)

    if (!load.isCompleted) {
      // if not completed immediately, show as pending
      model.subProp(_.loading).set(true)
      model.subProp(_.error).set(None)
      model.subProp(_.activities).set(Nil)
    }

    load.onComplete {
      case Success(loaded) =>
        import loaded._
        val stagedActivities = staged
        val allStravaActivities = strava
        println(s"loadActivities loaded staged: ${stagedActivities.size}, Strava: ${allStravaActivities.size}")

        val (stravaActivities, oldStravaActivities) = allStravaActivities.splitAt(UserContextService.normalCount)

        // without "withZoneSameInstant" the resulting time contained strange [SYSTEM] zone suffix
        val notBefore = if (showAll) ZonedDateTime.now().withZoneSameInstant(ZoneOffset.UTC) minusMonths 3
        else stravaActivities.map(a => a.id.startTime).min minusMonths 1

        def filterListed(activity: ActivityHeader, strava: Option[ActivityHeader]) = showAll || strava.isEmpty || strava.exists(_.id.startTime >= notBefore)
        def findMatchingStrava(ids: Seq[ActivityHeader], strava: Seq[ActivityHeader]): Seq[(ActivityHeader, Option[ActivityHeader])] = {
          ids.map( a => a -> strava.find(_.id isMatching a.id))
        }

        val neverBefore = alwaysIgnoreBefore(stravaActivities.map(_.id))

        // never display any activity which should be cleaned by UserCleanup
        val oldStagedActivities = stagedActivities.filter(_.id.startTime < neverBefore)
        val toCleanup = findMatchingStrava(oldStagedActivities, oldStravaActivities).flatMap { case (k,v) => v.map(k -> _)}
        val recentActivities = (stagedActivities diff toCleanup.map(_._1)).filter(_.id.startTime >= notBefore).sortBy(_.id.startTime)

        val recentToStrava = findMatchingStrava(recentActivities, allStravaActivities).filter((filterListed _).tupled).map(a => (Some(a._1), a._2))

        // list Strava activities which have no Mixtio storage counterpart
        val stravaOnlyAll = allStravaActivities.filterNot(a => recentActivities.exists(_.id.isMatchingExactly(a.id))).map(a => None -> Some(a))

        val stravaOnly = if (showAll) stravaOnlyAll else stravaOnlyAll.take(10)

        val toShow = (recentToStrava ++ stravaOnly).sortBy(a => a._1.orElse(a._2).get.id.startTime)
        val mostRecentStrava = stravaActivities.headOption.map(_.id.startTime)

        model.subProp(_.activities).set(toShow.map { case (act, actStrava) =>

          val ignored = actStrava.isDefined || mostRecentStrava.exists(s => act.exists(s >= _.id.startTime))
          ActivityRow(act, actStrava, !ignored)
        })
        model.subProp(_.loading).set(false)
      case Failure(exception) =>
        model.subProp(_.error).set(Some(exception))
        model.subProp(_.loading).set(false)
    }

  }

  override def handleState(state: SelectPageState.type): Unit = {}

  def unselectAll(): Unit = {
    model.subProp(_.activities).set {
      model.subProp(_.activities).get.map(_.copy(selected = false))
    }
  }

  private def selectedIds = {
    model.subProp(_.activities).get.filter(_.selected).flatMap(_.staged).map(_.id.id)
  }

  def deleteSelected(): Unit = {
    val fileIds = selectedIds
    userService.api.get.deleteActivities(fileIds).foreach { _ =>
      model.subProp(_.activities).set {
        model.subProp(_.activities).get.filter(!_.selected)
      }
    }
  }

  def uploadNewActivity() = {
    val selectedFiles = model.subSeq(_.uploads.selectedFiles).get.toSeq

    val userId = userService.userId.get

    val uploader = new FileUploader(Url(s"/rest/user/$userId/upload"))
    val uploadModel = uploader.upload("files", selectedFiles, extraData = Map(("timezone":js.Any) -> (TimeFormatting.timezone:js.Any)))
    uploadModel.listen { p =>
      model.subProp(_.uploads.state).set(p)
      for {
        response <- p.response
        responseJson <- response.text
      } {
        val activities = JsonUtils.read[Seq[ActivityHeader]](responseJson)
        // insert the activities into the list
        model.subProp(_.activities).set {
          val oldAct = model.subProp(_.activities).get
          val newAct = activities.filterNot(a => oldAct.exists(_.staged.exists(_.id == a.id)))
          val all = oldAct ++ newAct.map { a=>
            println(s"Add $a")
            ActivityRow(Some(a), None, selected = true)
          }
          all.sortBy(a => a.staged.orElse(a.strava).get.id.startTime)
        }
      }
    }
  }

  def importFromStrava(act: ActivityHeader): Unit = {
    val stravaImport = act.id.id match {
      case FileId.StravaId(stravaId) =>
        // TODO: some progress indication
        userService.api.get.importFromStrava(stravaId)
      case _ =>
        Future.failed(new NoSuchElementException)
    }
    stravaImport.onComplete { i =>
      println(s"Strava ${act.id.id} imported as $i")
      model.subProp(_.activities).set {
        model.subProp(_.activities).get.map { a =>
          if (a.strava.contains(act)) {
            i match {
              case Success(actId) =>
                a.copy(staged = Some(act.copy(id = actId)))
              case Failure(ex) =>
                val Regex = "^HTTP ERROR (\\d+):.*".r.unanchored
                val message = ex.getMessage match {
                  case Regex(code) => s"HTTP Error $code" // provide short message for expected causes
                  case x => x // unexpected cause - provide full error
                }
                a.copy(downloadState = message)
            }
          } else a
        }
      }
    }
  }

  def mergeAndEdit(): Unit = {
    val selected = selectedIds
    application.goTo(EditPageState(selected))
  }

  def gotoSettings(): Unit = {
    application.goTo(SettingsPageState)
  }

}

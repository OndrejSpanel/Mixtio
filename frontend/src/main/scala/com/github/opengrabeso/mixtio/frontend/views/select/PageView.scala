package com.github.opengrabeso.mixtio
package frontend
package views
package select

import java.time.temporal.ChronoUnit

import common.model._
import common.css._
import io.udash._
import io.udash.bootstrap.button.UdashButton
import io.udash.bootstrap.table.UdashTable
import io.udash.css._
import scalatags.JsDom.all._
import io.udash.bootstrap._
import BootstrapStyles._
import io.udash.bootstrap.collapse.UdashCollapse
import io.udash.bootstrap.progressbar.UdashProgressBar
import org.scalajs.dom

class PageView(
  model: ModelProperty[PageModel],
  presenter: PagePresenter,
) extends View with CssView with PageUtils with TimeFormatting with ActivityLink {
  val s = SelectPageStyles

  private val uploadButton = UdashButton()(_ => "Upload activity data...")
  private val settingsButton = UdashButton()(_ => "Settings")

  def nothingSelected: ReadableProperty[Boolean] = {
    model.subProp(_.activities).transform(!_.exists(_.selected))
  }

  private val deleteActivity = button(nothingSelected, s"Delete from $appName".toProperty)
  private val mergeAndEdit = button(
    nothingSelected,
    model.subProp(_.activities).transform(a => if (a.count(_.selected) > 1) "Merge and edit..." else "Edit...")
  )
  private val uncheckAll = button(nothingSelected, "Uncheck all".toProperty)

  private val filterCheckbox = Checkbox(model.subProp(_.showAll))()

  buttonOnClick(settingsButton) {presenter.gotoSettings()}
  buttonOnClick(uncheckAll)(presenter.unselectAll())
  buttonOnClick(mergeAndEdit) {presenter.mergeAndEdit()}
  buttonOnClick(deleteActivity) {presenter.deleteSelected()}
  buttonOnClick(uploadButton) {presenter.uploadNewActivity()}

  private val modelUploads = model.subModel(_.uploads)
  private val progress = modelUploads.subProp(_.state.bytesSent).combine(modelUploads.subProp(_.state.bytesTotal)) {
    (sent, total) => ((100 * sent) / total).toInt
  }
  private val progressBar = UdashProgressBar(progress, animated = true.toProperty)()

  import FileUploader.FileUploadState

  private val collapse = UdashCollapse()(_ => Seq(
    div(Card.card, Card.body, Background.color(Color.Light)) {
      val selectedFiles = model.subSeq(_.uploads.selectedFiles)

      div(
        FileInput(selectedFiles, acceptMultipleFiles = true.toProperty)("files"),
        h4("Selected files"),
        ul(
          repeat(modelUploads.subSeq(_.selectedFiles))(file =>
            li(s"${file.get.name} (${normalizeSize(file.get.size)})").render
          ),
          showIf(modelUploads.subSeq(_.selectedFiles).transform(_.isEmpty))(
            i("Select files to upload.").render
          )
        ),
        produce(modelUploads.subProp(_.state.state), checkNull = false) {
          case FileUploadState.NotStarted => span().render // info
          case FileUploadState.InProgress => progressBar.render // info
          case FileUploadState.Completed => label("Completed").render // success
          case FileUploadState.Cancelled => label("Cancelled").render // warning
          case FileUploadState.Failed => label("Failed").render // danger
        },
        br(),
        uploadButton
      )
    },

  ))

  private val uploadShowHideButton = UdashButton(
    options = Color.Primary.option
  )(_ => Seq[Modifier](collapse.toggleButtonAttrs(), "Upload..."))

  buttonOnClick(uploadShowHideButton) {collapse.toggle()}


  def getTemplate: Modifier = {

    // value is a callback
    type DisplayAttrib = TableFactory.TableAttrib[ActivityRow]
    val attribs = Seq[DisplayAttrib](
      TableFactory.TableAttrib("", (ar, p, nested) =>
        if (ar.staged.isDefined) {
          div(nested(checkbox(p.subProp(_.selected)))).render
        } else div().render
      ),
      TableFactory.TableAttrib("Time", (ar, _, _) => displayTimeRange(ar.id.startTime, ar.id.endTime).render),
      TableFactory.TableAttrib("Type", (ar, _, _) => ar.id.sportName.toString.render),
      TableFactory.TableAttrib("Distance", (ar, _, _) => displayDistance(ar.id.distance).render),
      TableFactory.TableAttrib("Duration", (ar, _, _) => displaySeconds(ChronoUnit.SECONDS.between(ar.id.startTime, ar.id.endTime).toInt).render),
      TableFactory.TableAttrib("Strava activity", { (ar, arProp, nested) =>
        div {
          // we are inside of `produce`, we can use `if` - `showIfElse` may have some performance advantage,
          // but this way it is easier to write and seems to work fine. Both `produce` and `showIfElse` are implemented
          // using `PropertyModifier`. The difference is `produce` is observing `activities` property, `showIfElse` could be more
          // granular, observing only `uploading` sub-property.
          // As `produce` must be called anyway because `activities` have changed, it should not matter.
          if (ar.uploading) {
            div(
              if (ar.uploadState.nonEmpty) s.error else s.uploading,
              if (ar.uploadState.nonEmpty) raw(ar.uploadState) else "Uploading..."
            ).render
          } else {
            ar.strava.map(i => hrefLink(i.id.id, i.id.shortName).render).toSeq
          }
        }.render
      }, Some("Strava")),
      TableFactory.TableAttrib("Data", (ar, _, _) => ar.header.describeData.render),
      TableFactory.TableAttrib("Source", { (ar, p, _) =>
        import io.udash.bootstrap.utils.UdashIcons.FontAwesome.Solid
        import io.udash.bootstrap.utils.UdashIcons.FontAwesome.Modifiers
        if (ar.strava.nonEmpty && ar.staged.isEmpty) {
          if (ar.downloadingStrava) {
            div(
              if (ar.downloadState.nonEmpty) s.error else s.uploading,
              if (ar.downloadState.nonEmpty) raw(ar.downloadState) else "Importing..."
            ).render
          } else {
            iconButton("Import from Strava to Mixtio")(Modifiers.Sizing.xs, Solid.cloudDownloadAlt)
              .onClick {
                p.subProp(_.downloadingStrava).set(true)
                p.subProp(_.downloadState).set("")
                presenter.importFromStrava(ar.strava.get)
              }.render
          }
        } else {
          div(
            s.wideMedia,
            ar.staged.map(h => hrefLink(h.id.id, h.id.shortName)).render
          ).render
        }
      }),
    )

    val table = UdashTable(model.subSeq(_.activities), striped = true.toProperty, bordered = true.toProperty, hover = true.toProperty, small = true.toProperty)(
      headerFactory = Some(TableFactory.headerFactory(attribs)),
      rowFactory = TableFactory.rowFactory(attribs)
    )

    div(
      s.container,
      div(Grid.row)(
        div(Grid.col)(uploadShowHideButton.render, collapse.render),
        div(Grid.col)(filterCheckbox.render, label("Show all": Modifier)),
        div(Grid.col)(settingsButton.render),
      ),

      div(
        showIfElse(model.subProp(_.loading))(
          p("Loading...").render,
          div(
            bind(model.subProp(_.error).transform(_.map(ex => p(s"Error loading activities ${ex.toString}")).orNull)),
            table.render
          ).render
        )
      ),
      div(
        mergeAndEdit.render,
        deleteActivity.render,
        uncheckAll.render
      )
    )
  }
}

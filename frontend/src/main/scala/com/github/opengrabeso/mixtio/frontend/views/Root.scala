package com.github.opengrabeso.mixtio
package frontend
package views

import routing._
import io.udash._
import io.udash.bootstrap._
import io.udash.bootstrap.button.{UdashButton, UdashButtonOptions}
import io.udash.component.ComponentId
import io.udash.css._
import common.css._
import org.scalajs.dom
import org.scalajs.dom.HTMLElement

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Root {

  case class PageModel(athleteName: String, userId: String)

  object PageModel extends HasModelPropertyCreator[PageModel]

  class PagePresenter(
    model: ModelProperty[PageModel],
    userContextService: services.UserContextService,
    application: Application[RoutingState]
  )(implicit ec: ExecutionContext) extends Presenter[RootState.type] {

    // start the login
    login()

    override def handleState(state: RootState.type): Unit = {}

    def login() = {
      val globalUserId = facade.UdashApp.currentUserId.orNull
      val globalAuthToken = facade.UdashApp.currentAuthToken.orNull
      val sessionId = facade.UdashApp.sessionId
      val ctx = userContextService.login(globalUserId, globalAuthToken, sessionId)
      userContextService.userName.foreach(_.foreach { name =>
        model.subProp(_.athleteName).set(name)
      })
      model.subProp(_.userId).set(ctx.userId)
    }

    def logout() = {
      if (model.subProp(_.userId).get != null) {
        println("Start logout")
        val oldName = userContextService.userName
        val oldId = userContextService.userId
        userContextService.logout().andThen {
          case Success(_) =>
            println(s"Logout done for $oldName ($oldId)")
            model.subProp(_.athleteName).set(null)
            model.subProp(_.userId).set(null)
            facade.UdashApp.currentUserId = scalajs.js.undefined

            val authCookies = Seq("authToken", "authRefreshToken", "authRefreshExpire", "authUserName", "authUserId")
            for (c <- authCookies) {
              MainJS.deleteCookie(c)
            }
            application.redirectTo(Url("/app"))
          case Failure(_) =>
        }
      }
    }

    def gotoMain() = {
      application.goTo(SelectPageState)
    }
  }


  class View(model: ModelProperty[PageModel], presenter: PagePresenter) extends ContainerView with CssView {

    import scalatags.JsDom.all._

    private val logoutButton = UdashButton(
      //buttonStyle = ButtonStyle.Default,
      options = UdashButtonOptions(block = true)
      , componentId = ComponentId("logout-button")
    )("Log Out")

    logoutButton.listen {
      case UdashButton.ButtonClickEvent(_, _) =>
        println("Logout submit pressed")
        presenter.logout()
    }


    val header: Seq[HTMLElement] = {
      val name = model.subProp(_.athleteName)
      val userId = model.subProp(_.userId)

      Seq(
        div(
          //GlobalStyles.header,
          id := "header",
          table(
            tbody(
              tr(
                td(a(href := "/", img(src := "static/stravaUpload64.png"))),
                td(
                  table(
                    tbody(
                      tr(td(a(
                        href := "/", appName,
                        onclick :+= {_: dom.Event =>
                          presenter.gotoMain()
                          true
                        }
                      ))),
                      tr(td(
                        "Athlete:",
                        produce(userId) { s =>
                          a(href := s"https://www.strava.com/athletes/$s", bind(name)).render
                        }
                      ))
                    )
                  )
                ),

                logoutButton.render
              )
            )
          )
        ).render
      )
    }


    val footer: Seq[HTMLElement] = Seq(
      div(
        //GlobalStyles.footer,
        id := "footer",
        a(
          href := "http://labs.strava.com/",
          id := "powered_by_strava",
          rel := "nofollow",
          img(
            GlobalStyles.stravaImg,
            attr("align") := "left",
            src :="static/api_logo_pwrdBy_strava_horiz_white.png",
          )
        ),
        p(
          GlobalStyles.footerText,
          " © 2016 - 2023 ",
          a(
            href := s"https://github.com/OndrejSpanel/$gitHubName",
            GlobalStyles.footerLink,
          ),
          "Ondřej Španěl",
          div()
        )
      ).render
    )

    // ContainerView contains default implementation of child view rendering
    // It puts child view into `childViewContainer`
    override def getTemplate: Modifier = div(
      // loads Bootstrap and FontAwesome styles from CDN
      UdashBootstrap.loadBootstrapStyles(),
      UdashBootstrap.loadFontAwesome(),

      BootstrapStyles.container,
      header,
      childViewContainer,
      footer
    )
  }

  class PageViewFactory(
    application: Application[RoutingState],
    userService: services.UserContextService,
  ) extends ViewFactory[RootState.type] {

    import scala.concurrent.ExecutionContext.Implicits.global

    override def create(): (View, Presenter[RootState.type]) = {
      val model = ModelProperty(PageModel(null, userService.userId.orNull))
      val presenter = new PagePresenter(model, userService, application)

      val view = new View(model, presenter)
      (view, presenter)
    }
  }

}

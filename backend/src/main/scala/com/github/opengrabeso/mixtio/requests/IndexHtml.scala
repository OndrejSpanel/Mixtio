package com.github.opengrabeso.mixtio
package requests

object IndexHtml extends DefineRequest("/") {

  def html(request: Request, resp: Response) = {
    val query = Option(request.queryString)
    val defaultPage = "/app"
    query.fold {
      resp.redirect(defaultPage)
    } { q =>
      resp.redirect(defaultPage + "?" + q)
    }
    Nil
  }
}
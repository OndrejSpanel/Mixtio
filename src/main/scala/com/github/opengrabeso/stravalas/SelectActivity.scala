package com.github.opengrabeso.stravalas
import spark.Request

object SelectActivity extends HtmlPage {
  override def html(request: Request) = {
    <html>
    <head>
      <title>Strava Split And Lap - select activity</title>
      <style>
        tr:nth-child(even) {{background-color: #f2f2f2}}
        tr:hover {{background-color: #f0f0e0}}
      </style>
    </head>
    <body>
      {val session = request.session()
    val code = request.queryParams("code")
    val auth = Main.stravaAuth(code)
    session.attribute("authToken", auth.token)
    session.attribute("mapboxToken", auth.mapboxToken)
    val activities = Main.lastActivities(auth.token)
    <p>Athlete:
      <b>
        {Main.athlete(auth.token)}
      </b>
    </p>
      <table>
        {for (act <- activities) yield {
        <tr>
          <td>
            {act.id}
          </td> <td>
          {act.sportName}
        </td> <td>
          <a href={act.link}>
            {act.name}
          </a>
        </td>
          <td>
            {Main.displayDistance(act.distance)}
            km</td> <td>
          {Main.displaySeconds(act.duration)}
        </td>
          <td>
            <form action="activity" method="get">
              <input type="hidden" name="activityId" value={act.id.toString}/>
              <input type="submit" value=">>"/>
            </form>
          </td>
        </tr>
      }}
      </table> :+ {
      cond (activities.length > 0) {
        <form action="activity" method="get">
          <p>Other activity Id:
            <input type="text" name="activityId" value={activities(0).id.toString}/>
            <input type="submit" value="Submit"/>
          </p>
        </form>
      }
    }}
    </body>
    </html>
  }
}

package com.github.opengrabeso.mixtio
package requests

object UdashApp  extends DefineRequest("/app") {
  /*
  sessionId is a time when the initial session page was rendered on the server. A new page will constitute a new session.
  */
  def html(request: Request, resp: Response) = withAuth(request, resp) { auth =>
    <html>
      <head>
        <title>{appName}</title>{headPrefix}
        <script src="frontend/script"></script> {/* scala.js compilation result */}
        <script src="frontend/dependencies"></script>{/* scala.js dependencies */}
        <link href="frontend/main.css" rel="stylesheet" /> {/* Udash generated stylesheet*/ }
        <script src="static/download.js"></script>
        <script src='https://api.mapbox.com/mapbox-gl-js/v1.0.0/mapbox-gl.js'></script>
        <link href='https://api.mapbox.com/mapbox-gl-js/v1.0.0/mapbox-gl.css' rel='stylesheet' />
        <script> // no secrets should be inserted here, this is readable by any end-user
          var currentUserId = '{auth.userId}';
          var currentAuthToken = getCookie('authToken');
          var sessionId = `{auth.sessionId}`; // time when the session was created on the server
          var mapBoxToken = `{auth.mapboxToken}`;
          appMain()
        </script>
      </head>
      <body>
        <div id="application"></div>
      </body>
    </html>
  }

}

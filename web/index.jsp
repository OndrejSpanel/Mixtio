<%--
  Created by IntelliJ IDEA.
  User: Ondra
  Date: 15.6.2016
  Time: 13:55
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ page import="com.github.opengrabeso.stravalas.*" %>
<html>
  <head>
    <title>Strava Split And Lap</title>
  </head>
  <body>
  <%
    String hostname = request.getServerName();
    int port = request.getServerPort();
    String scheme = request.getScheme();
    Main.SecretResult secret = Main.secret();
    String clientId = secret.appId();

    String serverUri = scheme + "://" + hostname + (port != 80 ? String.format(":%d", port) : "");
    String uri = "https://www.strava.com/oauth/authorize?";
    String action = uri + "client_id=" + clientId + "&response_type=code&redirect_uri=" + serverUri + "/selectActivity.jsp&scope=write,view_private";
  %>
  <h3>Work in progress, use at your own risk.</h3>
  <p>
    This tool allows you to split activity or edit lap information for it.
    It automatically detects places where you have stopped and allows you to create a split or lap there.
  </p>
  <h4>Working</h4>
  <ul>
    <li>Pauses listed, suggest laps and splits</li>
    <li>User can select events where laps should be inserted</li>
    <li>User can split activity and download individual parts</li>
    <li>Show average speed / tempo, autodetect activity type</li>
  </ul>
  <h4>
    Planned (not working yet)
  </h4>
  <ul>
    <li>User can change activity type (Run / Ride / Swim)</li>
    <li>Map (using MapBox)</li>
  </ul>
  <h4>Considering to add later</h4>
  <ul>
    <li>Delete old activity and upload the new version</li>
  </ul>
  <p>
    <i>Note: the original activity needs to be deleted in the process, therefore you will lose any comments and kudos you already have on it and your achievements will be recomputed.</i>
  </p>
  <% if (!clientId.isEmpty()) { %>
    <a href=<%=action%>><img src="static/ConnectWithStrava.png" alt="Connect with STRAVA"/></a>
  <% } else {%>
    <p>
      Error: <%= secret.error()%>
    </p>
  <% } %>


  </body>
</html>

import sbt.Keys.scalacOptions
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val commonSettings = Seq(
  organization := "com.github.ondrejspanel",
  version := "0.4.2-beta",
  scalaVersion := "2.12.13", // Scala.js 0.6.33 does not exist for 2.12.14
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  libraryDependencies += "org.scala-lang.modules" %%% "scala-collection-compat" % "2.2.0"
)

lazy val jsCommonSettings = Seq(
  scalacOptions ++= Seq("-P:scalajs:sjsDefinedByDefault")
)

val udashVersion = "0.8.2"

val bootstrapVersion = "4.3.1"

val udashJQueryVersion = "3.0.1"

// TODO: try to share
lazy val jvmLibs = Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",

  "io.udash" %% "udash-core" % udashVersion,
  "io.udash" %% "udash-rest" % udashVersion,
  "io.udash" %% "udash-rpc" % udashVersion,
  "io.udash" %% "udash-css" % udashVersion
)

lazy val jsLibs = libraryDependencies ++= Seq(
  "org.scalatest" %%% "scalatest" % "3.1.0" % "test",
  "org.scala-js" %%% "scalajs-dom" % "0.9.7",
  "org.querki" %%% "jquery-facade" % "1.2",

  "io.udash" %%% "udash-core" % udashVersion,
  "io.udash" %%% "udash-rest" % udashVersion,
  "io.udash" %%% "udash-rpc" % udashVersion,
  "io.udash" %%% "udash-css" % udashVersion,

  "io.udash" %%% "udash-bootstrap4" % udashVersion,
  "io.udash" %%% "udash-charts" % udashVersion,
  "io.udash" %%% "udash-jquery" % udashJQueryVersion,

  "com.zoepepper" %%% "scalajs-jsjoda" % "1.1.1",
  "com.zoepepper" %%% "scalajs-jsjoda-as-java-time" % "1.1.1"
)

lazy val jsDeps = jsDependencies ++= Seq(
  // "jquery.js" is provided by "udash-jquery" dependency
  "org.webjars" % "bootstrap" % bootstrapVersion / "bootstrap.bundle.js" minified "bootstrap.bundle.min.js" dependsOn "jquery.js",
  "org.webjars.npm" % "js-joda" % "1.10.1" / "dist/js-joda.js" minified "dist/js-joda.min.js"
)

lazy val commonLibs = Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
)

val jacksonVersion = "2.9.9"

lazy val sharedJs = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure).in(file("shared-js"))
  .disablePlugins(sbtassembly.AssemblyPlugin)
  .settings(commonSettings)
  .jvmSettings(libraryDependencies ++= jvmLibs)
  .jsSettings(
    jsCommonSettings,
    jsLibs,
    jsDeps
  )

lazy val sharedJs_JVM = sharedJs.jvm
lazy val sharedJs_JS = sharedJs.js

lazy val shared = (project in file("shared"))
  .dependsOn(sharedJs.jvm)
  .disablePlugins(sbtassembly.AssemblyPlugin)
  .settings(
    commonSettings,
    libraryDependencies ++= commonLibs
  )

lazy val core = (project in file("core"))
  .dependsOn(shared, sharedJs_JVM)
  .disablePlugins(sbtassembly.AssemblyPlugin)
  .settings(
    commonSettings,
    libraryDependencies += "com.fasterxml" % "aalto-xml" % "1.0.0",
    libraryDependencies ++= commonLibs
  )

lazy val fitConvert = (project in file("fit-convert"))
  .dependsOn(core)
  .settings(
    name := "FitConvert",
    commonSettings,
    libraryDependencies ++= commonLibs ++ jvmLibs,
    libraryDependencies += "commons-io" % "commons-io" % "2.1",
    libraryDependencies += "com.opencsv" % "opencsv" % "5.5"
  )


def inDevMode = true || sys.props.get("dev.mode").exists(value => value.equalsIgnoreCase("true"))

def addJavaScriptToServerResources(): Def.SettingsDefinition = {
  val optJs = if (inDevMode) fastOptJS else fullOptJS
  (Compile / resources) += (frontend / Compile / optJs).value.data
}

def addJSDependenciesToServerResources(): Def.SettingsDefinition = {
  val depJs = if (inDevMode) packageJSDependencies else packageMinifiedJSDependencies
  (Compile / resources) += (frontend / Compile / depJs).value
}

lazy val frontend = project.settings(
    commonSettings,
    jsCommonSettings,
    jsLibs
  ).enablePlugins(ScalaJSPlugin)
    .dependsOn(sharedJs_JS)

lazy val backend = (project in file("backend"))
  .dependsOn(core)
  .settings(

    addJavaScriptToServerResources(),
    addJSDependenciesToServerResources(),

    Compile / resourceGenerators += Def.task {
      val file = (Compile / resourceManaged).value / "config.properties"
      val contents = s"devMode=${inDevMode}"
      IO.write(file, contents)
      Seq(file)
    }.taskValue,

    commonSettings,

    libraryDependencies ++= commonLibs ++ jvmLibs ++ Seq(
      "com.google.http-client" % "google-http-client-appengine" % "1.39.0",
      "com.google.http-client" % "google-http-client-jackson2" % "1.39.0",
      "com.google.apis" % "google-api-services-storage" % "v1-rev171-1.25.0",
      //"com.google.apis" % "google-api-services-appengine" % "v1-rev20210618-1.32.1",
      //"com.google.appengine" % "appengine-api-1.0-sdk" % "1.9.89",

      // "javax.servlet" % "javax.servlet-api" % "4.0.1", // version 3.1.0 provided by the jetty-server should be fine
      "org.eclipse.jetty" % "jetty-server" % "9.3.18.v20170406",

      "com.google.appengine.tools" % "appengine-gcs-client" % "0.8.1" exclude("javax.servlet", "servlet.api"),
      "com.google.cloud" % "google-cloud-storage" % "1.118.0",
      "com.google.cloud" % "google-cloud-tasks" % "1.33.2",

      "com.fasterxml.jackson.core" % "jackson-core" % jacksonVersion,
      "com.fasterxml.jackson.core" % "jackson-databind" % jacksonVersion,

      "fr.opensagres.xdocreport.appengine-awt" % "appengine-awt" % "1.0.0",

      "com.sparkjava" % "spark-core" % "1.1.1" excludeAll ExclusionRule(organization = "org.eclipse.jetty"),
      "org.slf4j" % "slf4j-simple" % "1.6.1",
      "commons-fileupload" % "commons-fileupload" % "1.3.2",
      "com.jsuereth" %% "scala-arm" % "2.0" exclude(
        "org.scala-lang.plugins", "scala-continuations-library_" + scalaBinaryVersion.value
      ),
      "org.apache.commons" % "commons-math" % "2.1",
      "commons-io" % "commons-io" % "2.1"
    ),

    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "MANIFEST.MF") =>
        MergeStrategy.discard
      case PathList(ps @ _*) if ps.nonEmpty && Seq("io.netty.versions.properties", "module-info.class", "nowarn$.class", "nowarn.class").contains(ps.last) =>
        MergeStrategy.first
      case PathList("javax", "servlet", _*) =>
        MergeStrategy.first
      case PathList("META-INF", ps @ _*) if ps.nonEmpty && Seq("native-image.properties", "reflection-config.json").contains(ps.last) =>
        MergeStrategy.first
      case x =>
        // default handling for things like INDEX.LIST (see https://stackoverflow.com/a/46287790/16673)
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },

    assembly / assemblyJarName := "mixtio.jar",
    assembly / mainClass := Some("com.github.opengrabeso.mixtio.DevServer")
  )

lazy val root = (project in file(".")).aggregate(backend).settings(
  name := "Mixtio"
)
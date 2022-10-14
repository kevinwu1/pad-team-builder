lazy val buildSettings = Seq(
  organization := "idkwhatamidoing",
  version := "0.1.0",
  scalaVersion := "3.2.0"
)
Global / semanticdbEnabled := true
lazy val core = project
  .in(file("core"))
  .enablePlugins(ScalaJSPlugin) // this is needed apparently
  .settings(buildSettings)
  .settings(
    scalacOptions := Seq("Ymacro-debug-lite", "-deprecation"),
    name := "pad team builder",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.10.0-RC7"
  )

lazy val frontend = project
  .in(file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core)
  .settings(buildSettings)
  .settings(
    name := "pad team builder frontend",
    libraryDependencies ++= List(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.typesafe.play" %%% "play-json" % "2.10.0-RC7",
      "com.raquo" %%% "laminar" % "0.13.1" // Requires Scala.js >= 1.5.0
    ),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),
    scalaJSUseMainModuleInitializer := true
  )

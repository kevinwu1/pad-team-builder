lazy val buildSettings = Seq(
  organization := "idkwhatamidoing",
  version := "0.1.0",
  scalaVersion := "3.2.0"
)
Global / semanticdbEnabled := true

lazy val core = project
  .in(file("core"))
  .settings(buildSettings)
  .settings(
    target := {
      (ThisBuild / baseDirectory).value / "core" / "target-core"
    },
    scalacOptions ++= Seq(
      "Ymacro-debug-lite",
      "-deprecation"
    ),
    run / baseDirectory := (ThisBuild / baseDirectory).value / "core",
    run / fork := true,
    name := "pad team builder",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.10.0-RC7"
  )

lazy val corejs = project
  .in(file("core"))
  // https://github.com/scala-js/scala-js/issues/1925
  .enablePlugins(ScalaJSPlugin) // this is needed to create sjsir files
  .settings(buildSettings)
  .settings(
    // do not use := here or sbt cannot produce js output
    scalacOptions ++= Seq(
      "Ymacro-debug-lite",
      "-deprecation"
    ),
    name := "pad team builder",
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.typesafe.play" %%% "play-json" % "2.10.0-RC7",
    scalaJSUseMainModuleInitializer := true
  )

lazy val frontend = project
  .in(file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(corejs)
  .settings(buildSettings)
  .settings(
    name := "pad team builder frontend",
    libraryDependencies ++= List(
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
      "com.typesafe.play" %%% "play-json" % "2.10.0-RC7",
      "com.raquo" %%% "laminar" % "0.13.1" // Requires Scala.js >= 1.5.0
    ),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),
    scalaJSUseMainModuleInitializer := true
  )

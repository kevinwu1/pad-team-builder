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
    scalacOptions ++= Seq(
      "Ymacro-debug-lite",
      "-deprecation"
    ),
    run / baseDirectory := (ThisBuild / baseDirectory).value / "core",
    run / fork := true,
    name := "pad team builder",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.typesafe.play" %% "play-json" % "2.10.0-RC7",
      "com.lihaoyi" %% "upickle" % "2.0.0"
    )
  )

lazy val corejs = project
  .in(file("core"))
  // https://github.com/scala-js/scala-js/issues/1925
  .enablePlugins(ScalaJSPlugin) // this is needed to create sjsir files
  .settings(buildSettings)
  .settings(
    target := {
      (ThisBuild / baseDirectory).value / "core" / "target-json"
    },
    // do not use := here or sbt cannot produce js output
    scalacOptions ++= Seq(
      "Ymacro-debug-lite",
      "-deprecation"
    ),
    javaOptions += "-Xss1048576",
    javaOptions += "-Xmx8G",
    name := "pad team builder",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      // scala-js dependency needs triple percent
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
      "com.typesafe.play" %%% "play-json" % "2.10.0-RC7",
      "com.lihaoyi" %%% "upickle" % "2.0.0"
    )
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
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("ptbFrontend.PadTeamBuilder"),
    customFast := {
      val fastLink = (Compile / fastLinkJS).value
      println(java.nio.file.Paths.get(".").toAbsolutePath)
      val outputDir = "./static/"
      java.nio.file.Files.createDirectories(java.nio.file.Paths.get(outputDir));
      new java.io.File(
        "frontend/target/scala-3.2.0/pad-team-builder-frontend-fastopt"
      )
        .listFiles()
        .foreach(file => {
          java.nio.file.Files
            .copy(
              file.toPath,
              new java.io.File(outputDir, file.name).toPath,
              java.nio.file.StandardCopyOption.REPLACE_EXISTING
            )
        })
      new java.io.File(
        "frontend/src/main/resources/"
      )
        .listFiles()
        .foreach(file => {
          java.nio.file.Files
            .copy(
              file.toPath,
              new java.io.File(outputDir, file.name).toPath,
              java.nio.file.StandardCopyOption.REPLACE_EXISTING
            )
        })
    },
    customFull := {
      val fullLink = (Compile / fullLinkJS).value
      println(java.nio.file.Paths.get(".").toAbsolutePath)
      val outputDir = "./static/"
      java.nio.file.Files
        .createDirectories(java.nio.file.Paths.get(outputDir));
      new java.io.File(
        "frontend/target/scala-3.2.0/pad-team-builder-frontend-opt"
      )
        .listFiles()
        .foreach(file => {
          java.nio.file.Files
            .copy(
              file.toPath,
              new java.io.File(outputDir, file.name).toPath,
              java.nio.file.StandardCopyOption.REPLACE_EXISTING
            )
        })
      new java.io.File(
        "frontend/src/main/resources/"
      )
        .listFiles()
        .foreach(file => {
          java.nio.file.Files
            .copy(
              file.toPath,
              new java.io.File(outputDir, file.name).toPath,
              java.nio.file.StandardCopyOption.REPLACE_EXISTING
            )
        })
    }
  )

lazy val customFast = taskKey[Unit]("Fast link")
lazy val customFull = taskKey[Unit]("Full link")

Global / cancelable := true

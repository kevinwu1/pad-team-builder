import mill._, scalalib._

trait MyModule extends ScalaModule {
  def scalaVersion = "3.4.1"
}

object core extends MyModule {
  def ivyDeps = Agg(
    ivy"com.typesafe.play::play-json:2.10.0-RC7",
    ivy"com.lihaoyi::upickle:2.0.0"
  )
}

object frontend extends MyModule {
  def moduleDeps = Seq(core)
  def ivyDeps = Agg(
    ivy"com.typesafe.play::play-json:2.10.0-RC7",
    ivy"com.lihaoyi::upickle:2.0.0"
  )
}

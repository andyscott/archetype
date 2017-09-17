lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(`core`, `scalameta-fixedpoint`, `scalameta-adjunct`)

lazy val V = new {
  lazy val scalacheck               = "1.13.4"
  lazy val scalameta                = "2.0.0-RC1"
  lazy val cats                     = "1.0.0-MF"
}

lazy val `core` = module("core")
  .dependsOn(`scalameta-fixedpoint`)

lazy val `scalameta-fixedpoint` = module("scalameta-fixedpoint")
  .dependsOn(`scalameta-adjunct`)
  .settings(libraryDependencies ++= Seq(
    "org.scalameta"              %% "scalameta"                 % V.scalameta,
    "org.typelevel"              %% "cats-core"                 % V.cats
  ))
  .settings(libraryDependencies ++= Seq(
    "org.scalacheck"             %% "scalacheck"                % V.scalacheck
  ).map(_ % "test"))

lazy val `scalameta-adjunct` = module("scalameta-adjunct")
  .settings(libraryDependencies ++= Seq(
    "org.scalameta"              %% "scalameta"                 % V.scalameta
  ))
  .settings(libraryDependencies ++= Seq(
    "org.scalacheck"             %% "scalacheck"                % V.scalacheck
  ).map(_ % "test"))

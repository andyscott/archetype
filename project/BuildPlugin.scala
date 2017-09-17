package xyz.anamorph

import sbt.Keys._
import sbt._

import de.heikoseeberger.sbtheader.AutomateHeaderPlugin
import de.heikoseeberger.sbtheader.HeaderPlugin

object BuildPlugin extends AutoPlugin {
  import HeaderPlugin.autoImport._

  override def requires = plugins.JvmPlugin && HeaderPlugin
  override def trigger = allRequirements

  object autoImport extends BuildDSL

  override def projectSettings = Seq(
    organization := "xyz.anamorph",

    fork in run := true,
    fork in Test := true,
    parallelExecution in Test := false,
    outputStrategy := Some(StdoutOutput),
    connectInput in run := true,
    cancelable in Global := true,

    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3"),

    headerLicense := Some(HeaderLicense.Custom(
      """|Copyright (C) 2017 Andy Scott
         |Archetype is licensed under the Apache License 2.0
         |""".stripMargin))
  ) ++ AutomateHeaderPlugin.projectSettings


}

sealed trait BuildDSL {

  def module(modName: String): Project =
    Project(modName, file(s"""modules/$modName"""))
      .settings(name := s"archetype-$modName")

  lazy val noPublishSettings = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false)

}

/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint

import cats.Id

import org.scalacheck._
import org.scalacheck.Prop._

import scala.meta.dialects._
import scala.meta.adjunct.all._

object TreeChecks extends Properties("trees") {

  val dialect = Scala212.copy(allowToplevelTerms = true)

  val snippets: List[String] = List(
    "1 + 1",
    "1 + 2 + 3",
    "val x = 1",
    "val x = 1 + 2")

  val roundTripTree: Id ~> Id = hylo(algebras.lowerTree, algebras.liftTree)

  snippets.foreach(snippet =>
    property(s"round trip: $snippet") = roundTrip(snippet))

  def roundTrip(snippet: String): Prop =
    dialect(snippet).parse[Source].fold(_ => Prop.falsified, parsed => {
      val res = roundTripTree(parsed)
      parsed.structure ?= res.structure
    })

}

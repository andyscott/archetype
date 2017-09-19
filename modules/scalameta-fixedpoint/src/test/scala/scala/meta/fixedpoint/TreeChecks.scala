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

  private val snippets: List[String] = List(
    "trait Foo",
    "class Bar",
    "object Baz",
    "package Fleeb",
    "package object Plumbus"
  )

  private val termSnippets: List[String] = List(
    "val x = 1",
    "val x = 1 + 2",
    "var x = 1 + 2 + 3",
    "def x = 1 + 2 + 3 + 4"
  )

  private val roundTripTree: Id ~> Id = hylo(algebras.lowerTree, algebras.liftTree)

  snippets.foreach(snippet =>
    property(s"round trip: $snippet") = roundTrip(snippet, Scala212))

  termSnippets.foreach(snippet =>
    property(s"round trip term: $snippet") = roundTrip(snippet,
      Scala212.copy(allowToplevelTerms = true)))

  private def roundTrip(snippet: String, dialect: Dialect): Prop =
    dialect(snippet).parse[Source].fold(
      e => Prop.falsified :| s"unable to parse original source: $e",
      parsed => {
        val res = roundTripTree(parsed)
        parsed.structure ?= res.structure
      })

}

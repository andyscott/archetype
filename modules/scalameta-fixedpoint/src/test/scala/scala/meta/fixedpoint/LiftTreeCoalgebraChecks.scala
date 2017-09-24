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

import data.HFix
import trees._

object LiftTreeCoalgebraChecks extends Properties("liftTree") {

  val dialect = Scala212

  val liftTreeAna: Id ~>: HFix[TreeF, ?] = Corecursive[HFix[TreeF, ?]]
    .ana(algebras.liftTree)

  type FixedTree = HFix[TreeF, Tree]

  private val snippets: List[(String, FixedTree)] = List(
    (
      "class X { val x = 1 }",

      HFix(SourceF(List(
        HFix(DefnF.ClassF(
          Nil,
          TypeF.NameF("X").fix,
          Nil,
          HFix(CtorF.PrimaryF(Nil, NameF.AnonymousF.fix, Nil)),
          HFix(TemplateF(
            Nil,
            Nil,
            HFix(SelfF(NameF.AnonymousF.fix, None)),
            List(
              HFix(DefnF.ValF(
                Nil,
                List(PatF.VarF(TermF.NameF("x").fix).fix),
                None,
                LitF.IntF(1).fix))))))))))
    )
  )

  snippets.foreach { case (snippet, expected) =>
    property(s"liftTree: $snippet") = checkLiftTree(snippet, expected, Scala212) }

  private def checkLiftTree(snippet: String, expected: FixedTree, dialect: Dialect): Prop =
    dialect(snippet).parse[Source].fold(
      e => Prop.falsified :| s"unable to parse original source: $e",
      parsed => {
        val res = liftTreeAna(parsed: Tree)
        res ?= expected
      })

}

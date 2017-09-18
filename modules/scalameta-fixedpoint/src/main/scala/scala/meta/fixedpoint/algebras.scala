/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint

import cats.Id

import trees._

package object algebras {

  type LiftTree  = Coalgebra[TreeF, Id]
  type LowerTree = Algebra[TreeF, Id]

  /** Coalgebra to lift a regular `Tree` to a fixed point `TreeF` */
  val liftTree: LiftTree = new LiftTree {
    def apply[A](tree: Id[A]): TreeF[Id, A] = (tree: A) match {
      case t: Source          => SourceF[Id](t.stats)
      case t: Term.ApplyInfix => TermF.ApplyInfixF[Id](t.lhs, t.op, t.targs, t.args)
      case t: Term.Name       => TermF.NameF(t.value)
      case t: Lit.Int         => LitF.IntF(t.value)
      case t: Defn.Val        => DefnF.ValF[Id](t.mods, t.pats, t.decltpe, t.rhs)
      case t: Pat.Var         => PatF.VarF[Id](t.name)
      case _                  =>
        sys.error(s"tree node type ${tree.getClass} is not yet accounted for")
    }

    // * begin giant sigh *
    // ...this is needed to get the above to typecheck...
    // * end of giant sigh *
    @inline private[this] implicit def `y u no typecheck?`[A](tree: TreeF[Id, Tree]): TreeF[Id, A] =
      tree.asInstanceOf[TreeF[Id, A]]
  }

  /** Algebra to lower a fixed point `TreeF` to a regular `Tree` */
  val lowerTree: LowerTree = new LowerTree {
    def apply[A](tree: TreeF[Id, A]): Id[A] = tree match {
      case t: SourceF[Id]           => Source(t.stats)
      case t: TermF.ApplyInfixF[Id] => Term.ApplyInfix(t.lhs, t.op, t.targs, t.args)
      case t: TermF.NameF           => Term.Name(t.value)
      case t: LitF.IntF             => Lit.Int(t.value)
      case t: DefnF.ValF[Id]        => Defn.Val(t.mods, t.pats, t.decltpe, t.rhs)
      case t: PatF.VarF[Id]         => Pat.Var(t.name)
      case _                        => ???
    }
  }

}

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

      case _: Name.Anonymous     => NameF.AnonymousF
      case t: Name.Indeterminate => NameF.IndeterminateF(t.value)

      case t: Lit.Int            => LitF.IntF(t.value)

      case t: Term.ApplyInfix    => TermF.ApplyInfixF[Id](t.lhs, t.op, t.targs, t.args)
      case t: Term.Name          => TermF.NameF(t.value)

      case t: Type.Name          => TypeF.NameF(t.value)
      case t: Type.Select        => TypeF.SelectF[Id](t.qual, t.name)

      case t: Pat.Var            => PatF.VarF[Id](t.name)

      case t: Defn.Val           => DefnF.ValF[Id](t.mods, t.pats, t.decltpe, t.rhs)
      case t: Defn.Var           => DefnF.VarF[Id](t.mods, t.pats, t.decltpe, t.rhs)
      case t: Defn.Def           => DefnF.DefF[Id](t.mods, t.name, t.tparams, t.paramss, t.decltpe, t.body)
      case t: Defn.Macro         => DefnF.MacroF[Id](t.mods, t.name, t.tparams, t.paramss, t.decltpe, t.body)
      case t: Defn.Type          => DefnF.TypeF[Id](t.mods, t.name, t.tparams, t.body)
      case t: Defn.Class         => DefnF.ClassF[Id](t.mods, t.name, t.tparams, t.ctor, t.templ)
      case t: Defn.Trait         => DefnF.TraitF[Id](t.mods, t.name, t.tparams, t.ctor, t.templ)
      case t: Defn.Object        => DefnF.ObjectF[Id](t.mods, t.name, t.templ)

      case t: Pkg                => PkgF[Id](t.ref, t.stats)
      case t: Pkg.Object         => PkgF.ObjectF[Id](t.mods, t.name, t.templ)

      case t: Ctor.Primary       => CtorF.PrimaryF[Id](t.mods, t.name, t.paramss)

      case t: Self               => SelfF[Id](t.name, t.decltpe)
      case t: Template           => TemplateF[Id](t.early, t.inits, t.self, t.stats)

      case t: Source             => SourceF[Id](t.stats)

      case _                     =>
        sys.error(s"tree node type ${tree.getClass} is not yet liftable")
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

      case    NameF.AnonymousF      => Name.Anonymous()
      case t: NameF.IndeterminateF  => Name.Indeterminate(t.value)

      case t: LitF.IntF             => Lit.Int(t.value)

      case t: TermF.ApplyInfixF[Id] => Term.ApplyInfix(t.lhs, t.op, t.targs, t.args)
      case t: TermF.NameF           => Term.Name(t.value)

      case t: TypeF.NameF           => Type.Name(t.value)
      case t: TypeF.SelectF[Id]     => Type.Select(t.qual, t.name)

      case t: PatF.VarF[Id]         => Pat.Var(t.name)

      case t: DefnF.ValF[Id]        => Defn.Val(t.mods, t.pats, t.decltpe, t.rhs)
      case t: DefnF.VarF[Id]        => Defn.Var(t.mods, t.pats, t.decltpe, t.rhs)
      case t: DefnF.DefF[Id]        => Defn.Def(t.mods, t.name, t.tparams, t.paramss, t.decltpe, t.body)
      case t: DefnF.MacroF[Id]      => Defn.Macro(t.mods, t.name, t.tparams, t.paramss, t.decltpe, t.body)
      case t: DefnF.TypeF[Id]       => Defn.Type(t.mods, t.name, t.tparams, t.body)
      case t: DefnF.ClassF[Id]      => Defn.Class(t.mods, t.name, t.tparams, t.ctor, t.templ)
      case t: DefnF.TraitF[Id]      => Defn.Trait(t.mods, t.name, t.tparams, t.ctor, t.templ)
      case t: DefnF.ObjectF[Id]     => Defn.Object(t.mods, t.name, t.templ)

      case t: PkgF[Id]              => Pkg(t.ref, t.stats)
      case t: PkgF.ObjectF[Id]      => Pkg.Object(t.mods, t.name, t.templ)

      case t: CtorF.PrimaryF[Id]    => Ctor.Primary(t.mods, t.name, t.paramss)

      case t: SelfF[Id]             => Self(t.name, t.decltpe)
      case t: TemplateF[Id]         => Template(t.early, t.inits, t.self, t.stats)

      case t: SourceF[Id]           => Source(t.stats)

      case _                        =>
        sys.error(s"tree node type ${tree.getClass} is not yet lowerable")
    }
  }

}

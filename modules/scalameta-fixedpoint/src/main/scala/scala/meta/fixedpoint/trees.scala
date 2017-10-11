/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint
package trees

import cats.Applicative
import cats.Id
import cats.instances.all._
import cats.syntax.all._

import data.HFix

sealed trait TreeF[+A[_], +I]

object TreeF extends TreeFInstances0 {
  /** This lets you call `.fix` on the leaf node trees with `A[_] = Nothing` */
  implicit final class FixTreeFOps[I](val tree: TreeF[HFix[TreeF, ?], I]) extends AnyVal {
    def fix[II >: I]: HFix[TreeF, II] = HFix.hfix[TreeF, II](tree)
  }
}

sealed trait RefF[A[_], I <: Ref] extends TreeF[A, I]
sealed trait StatF[A[_], I <: Stat] extends TreeF[A, I]

sealed trait NameF[A[_], I <: Name] extends RefF[A, I]
object NameF {
  final case object AnonymousF extends NameF[Nothing, Name.Anonymous]
  final case class IndeterminateF(
    value: String
  ) extends NameF[Nothing, Name.Indeterminate]
}

sealed trait LitF[A[_], I <: Lit] extends TermF[A, I] with PatF[A, I] with TypeF[A, I]
object LitF {
  final case object NullF                                 extends LitF[Nothing, Lit.Null]
  final case class  IntF    (value : scala.Int)           extends LitF[Nothing, Lit.Int]
  final case class  DoubleF (format: scala.Predef.String) extends LitF[Nothing, Lit.Double]
  final case class  FloatF  (format: scala.Predef.String) extends LitF[Nothing, Lit.Float]
  final case class  ByteF   (value : scala.Byte)          extends LitF[Nothing, Lit.Byte]
  final case class  ShortF  (value : scala.Short)         extends LitF[Nothing, Lit.Short]
  final case class  CharF   (value : scala.Char)          extends LitF[Nothing, Lit.Char]
  final case class  LongF   (value : scala.Long)          extends LitF[Nothing, Lit.Long]
  final case class  BooleanF(value : scala.Boolean)       extends LitF[Nothing, Lit.Boolean]
  final case object Unit                                  extends LitF[Nothing, Lit.Unit]
  final case class  StringF (value : scala.Predef.String) extends LitF[Nothing, Lit.String]
  final case class  SymbolF (value : scala.Symbol)        extends LitF[Nothing, Lit.Symbol]
}

sealed trait TermF[A[_], I <: Term] extends StatF[A, I]
object TermF {
  sealed trait RefF[A[_], I <: Term.Ref] extends TermF[A, I] with trees.RefF[A, I]
  final case class ThisF[A[_]](qual: A[Name]) extends RefF[A, Term.This]
  final case class SuperF[A[_]](thisp: A[Name], superp: A[Name]) extends RefF[A, Term.Super]
  final case class NameF(value: String) extends trees.NameF[Nothing, Term.Name] with RefF[Nothing, Term.Name] with PatF[Nothing, Term.Name]
  // ...
  final case class ApplyInfixF[A[_]](lhs: A[Term], op: A[Term.Name], targs: List[A[Type]], args: List[A[Term]]) extends TermF[A, Term.ApplyInfix]
  // ...
}

sealed trait TypeF[A[_], I <: Type] extends TreeF[A, I]
object TypeF {
  sealed trait RefF[A[_], I <: Type.Ref] extends TypeF[A, I] with trees.RefF[A, I]

  final case class NameF(value: String) extends trees.NameF[Nothing, Type.Name] with RefF[Nothing, Type.Name]
  final case class SelectF[A[_]](qual: A[Term.Ref], name: A[Type.Name]) extends RefF[A, Type.Select]
  // ...
  final case class ParamF[A[_]](
    mods   : List[A[Mod]],
    name   : A[Name],
    tparams: List[A[Type.Param]],
    tbounds: A[Type.Bounds],
    vbounds: List[A[Type]],
    cbounds: List[A[Type]]) extends MemberF[A, Type.Param]
  // ...
}

sealed trait PatF[A[_], I <: Pat] extends TreeF[A, I]
object PatF {
  final case class VarF[A[_]](name: A[Term.Name]) extends PatF[A, Pat.Var] with MemberF.TermF[A, Pat.Var]
  // ...
}

sealed trait MemberF[A[_], I <: Member] extends TreeF[A, I]
object MemberF {
  sealed trait TermF[A[_], I <: Member.Term] extends MemberF[A, I]
  sealed trait TypeF[A[_], I <: Member.Type] extends MemberF[A, I]
}

sealed trait DeclF[A[_], I <: Decl] extends StatF[A, I]
object DeclF {
  final case class ValF[A[_]](
    mods   : List[A[Mod]],
    pats   : List[A[Pat]],
    decltpe: A[Type]
  ) extends DeclF[A, Decl.Val]

  final case class VarF[A[_]](
    mods   : List[A[Mod]],
    pats   : List[A[Pat]],
    decltpe: A[Type]
  ) extends DeclF[A, Decl.Var]

  final case class DefF[A[_]](
    mods   : List[A[Mod]],
    name   : A[Term.Name],
    tparams: List[A[Type.Param]],
    paramss: List[List[A[Term.Param]]],
    decltpe: A[Type]
  ) extends DeclF[A, Decl.Def] with MemberF.TermF[A, Decl.Def]

  final case class TypeF[A[_]](
    mods: List[A[Mod]],
    name: A[Type.Name],
    tparams: List[A[Type.Param]],
    bounds: A[Type.Bounds]
  ) extends DeclF[A, Decl.Type] with MemberF.TypeF[A, Decl.Type]
}

sealed trait DefnF[A[_], I <: Defn] extends StatF[A, I]
object DefnF {

  final case class ValF[A[_]](
    mods   : List[A[Mod]],
    pats   : List[A[Pat]],
    decltpe: Option[A[Type]],
    rhs    : A[Term]
  ) extends DefnF[A, Defn.Val]

  final case class VarF[A[_]](
    mods   : List[A[Mod]],
    pats   : List[A[Pat]],
    decltpe: Option[A[Type]],
    rhs    : Option[A[Term]]
  ) extends DefnF[A, Defn.Var]

  final case class DefF[A[_]](
    mods   : List[A[Mod]],
    name   : A[Term.Name],
    tparams: List[A[Type.Param]],
    paramss: List[List[A[Term.Param]]],
    decltpe: Option[A[Type]],
    body   : A[Term]
  ) extends DefnF[A, Defn.Def] with MemberF.TermF[A, Defn.Def]

  final case class MacroF[A[_]](
    mods   : List[A[Mod]],
    name   : A[Term.Name],
    tparams: List[A[Type.Param]],
    paramss: List[List[A[Term.Param]]],
    decltpe: Option[A[Type]],
    body   : A[Term]
  ) extends DefnF[A, Defn.Macro] with MemberF.TermF[A, Defn.Macro]

  final case class TypeF[A[_]](
    mods   : List[A[Mod]],
    name   : A[Type.Name],
    tparams: List[A[Type.Param]],
    body   : A[Type]
  ) extends DefnF[A, Defn.Type] with MemberF.TypeF[A, Defn.Type]

  final case class ClassF[A[_]](
    mods   : List[A[Mod]],
    name   : A[Type.Name],
    tparams: List[A[Type.Param]],
    ctor   : A[Ctor.Primary],
    templ  : A[Template]
  ) extends DefnF[A, Defn.Class] with MemberF.TypeF[A, Defn.Class]

  final case class TraitF[A[_]](
    mods   : List[A[Mod]],
    name   : A[Type.Name],
    tparams: List[A[Type.Param]],
    ctor   : A[Ctor.Primary],
    templ  : A[Template]
  ) extends DefnF[A, Defn.Trait] with MemberF.TypeF[A, Defn.Trait]

  final case class ObjectF[A[_]](
    mods   : List[A[Mod]],
    name   : A[Term.Name],
    templ  : A[Template]
  ) extends DefnF[A, Defn.Object] with MemberF.TermF[A, Defn.Object]

}

final case class PkgF[A[_]](
  ref  : A[Term.Ref],
  stats: List[A[Stat]]
) extends MemberF.TermF[A, Pkg] with StatF[A, Pkg]

object PkgF {
  final case class ObjectF[A[_]](
    mods : List[A[Mod]],
    name : A[Term.Name],
    templ: A[Template]
  ) extends MemberF.TermF[A, Pkg.Object] with StatF[A, Pkg.Object]
}

sealed trait CtorF[A[_], I <: Ctor] extends TreeF[A, I] with MemberF[A, I]
object CtorF {
  final case class PrimaryF[A[_]](
    mods: List[A[Mod]],
    name: A[Name],
    paramss: List[List[A[Term.Param]]]
  ) extends CtorF[A, Ctor.Primary]

  final case class SecondaryF[A[_]](
    mods: List[A[Mod]],
    name: A[Name],
    paramss: List[List[A[Term.Param]]],
    init: A[Init],
    stats: List[A[Stat]]
  ) extends CtorF[A, Ctor.Secondary] with StatF[A, Ctor.Secondary]
}

final case class InitF[A[_]](tpe: A[Type], name: A[Name], argss: List[List[A[Term]]]) extends RefF[A, Init]

final case class SelfF[A[_]](name: A[Name], decltpe: Option[A[Type]]) extends MemberF[A, Self]

final case class TemplateF[A[_]](
  early: List[A[Stat]],
  inits: List[A[Init]],
  self : A[Self],
  stats: List[A[Stat]]
) extends TreeF[A, Template]

sealed trait ModF[A[_], I <: Mod] extends TreeF[A, I]
object ModF {

  final case class AnnotF    [A[_]](init  : A[Init]) extends ModF[A, Mod.Annot]
  final case class PrivateF  [A[_]](within: A[Ref])  extends ModF[A, Mod.Private]
  final case class ProtectedF[A[_]](within: A[Ref])  extends ModF[A, Mod.Protected]

  final case object ImplicitF      extends ModF[Nothing, Mod.Implicit]
  final case object FinalF         extends ModF[Nothing, Mod.Final]
  final case object SealedF        extends ModF[Nothing, Mod.Sealed]
  final case object OverrideF      extends ModF[Nothing, Mod.Override]
  final case object CaseF          extends ModF[Nothing, Mod.Case]
  final case object AbstractF      extends ModF[Nothing, Mod.Abstract]
  final case object CovariantF     extends ModF[Nothing, Mod.Covariant]
  final case object ContravariantF extends ModF[Nothing, Mod.Contravariant]
  final case object LazyF          extends ModF[Nothing, Mod.Lazy]
  final case object ValParamF      extends ModF[Nothing, Mod.ValParam]
  final case object VarParamF      extends ModF[Nothing, Mod.VarParam]
  final case object InlineF        extends ModF[Nothing, Mod.Inline]
}

sealed trait EnumeratorF[A[_], I <: Enumerator] extends TreeF[A, I]
object EnumeratorF {
  final case class GeneratorF[A[_]](
    pat: A[Pat],
    rhs: A[Term]
  ) extends EnumeratorF[A, Enumerator.Generator]

  final case class ValF[A[_]](
    pat: A[Pat],
    rhs: A[Term]
  ) extends EnumeratorF[A, Enumerator.Val]

  final case class GuardF[A[_]](
    cond: A[Term]
  ) extends EnumeratorF[A, Enumerator.Guard]
}

final case class ImportF[A[_]](
  importers: List[A[Importer]]
) extends StatF[A, Import]

final case class ImporterF[A[_]](
  ref      : A[Term.Ref],
  importees: List[A[Importee]]
)extends TreeF[A, Importer]

sealed trait ImporteeF[A[_], I <: Importee] extends TreeF[A, I] with RefF[A, I]
object ImporteeF {
  final case object WildcardF extends ImporteeF[Nothing, Importee.Wildcard]

  final case class NameF[A[_]](
    name: A[Name]
  ) extends ImporteeF[A, Importee.Name]

  final case class RenameF[A[_]](
    name  : A[Name],
    rename: A[Name]
  ) extends ImporteeF[A, Importee.Rename]

  final case class UnimportF[A[_]](
    name: A[Name]
  ) extends ImporteeF[A, Importee.Unimport]
}

final case class CaseF[A[_]](
  pat : A[Pat],
  cond: Option[A[Term]],
  body: A[Term]) extends TreeF[A, Case]

final case class SourceF[A[_]](
  stats: List[A[Stat]]
) extends TreeF[A, Source]

private[trees] sealed trait TreeFInstances0 {

  implicit val traverseHTreeF: TraverseH[TreeF] = new TraverseH[TreeF] {

    import scala.{ unchecked => χ }

    def traverseH[F[_], A[_], B[_]](f: A ~>: (F ∘ B)#λ)(implicit F: Applicative[F]): TreeF[A, ?] ~>: (F ∘ TreeF[B, ?])#λ =
      new (TreeF[A, ?] ~>: (F ∘ TreeF[B, ?])#λ) {
        def apply[Z](tree: TreeF[A, Z]): F[TreeF[B, Z]] = tree match {

          // Note: Id is used where A should be used because using A
          // causes the compiler to crash... and these types don't get
          // checked anyway due to erasure


          case t: NameF[B @χ, Z @χ] => F.pure(t)

          case t: LitF[B @χ, Z @χ]  => F.pure(t)

          case t: TermF.ApplyInfixF[Id @χ] =>
            (
              f(t.lhs),
              f(t.op),
              t.targs.traverse(f(_)),
              t.args.traverse(f(_))
            ) mapN TermF.ApplyInfixF[B]

          case t: PatF.VarF[Id @χ] =>
            f(t.name) map PatF.VarF[B]


          case t: DefnF.ValF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              t.pats.traverse(f(_)),
              t.decltpe.traverse(f(_)),
              f(t.rhs)
            ) mapN DefnF.ValF[B]

          case t: DefnF.VarF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              t.pats.traverse(f(_)),
              t.decltpe.traverse(f(_)),
              t.rhs.traverse(f(_))
            ) mapN DefnF.VarF[B]

          case t: DefnF.DefF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              f(t.name),
              t.tparams.traverse(f(_)),
              t.paramss.traverse(_.traverse(f(_))),
              t.decltpe.traverse(f(_)),
              f(t.body)
            ) mapN DefnF.DefF[B]

          case t: DefnF.MacroF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              f(t.name),
              t.tparams.traverse(f(_)),
              t.paramss.traverse(_.traverse(f(_))),
              t.decltpe.traverse(f(_)),
              f(t.body)
            ) mapN DefnF.MacroF[B]

          case t: DefnF.TypeF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              f(t.name),
              t.tparams.traverse(f(_)),
              f(t.body)
            ) mapN DefnF.TypeF[B]

          case t: DefnF.ClassF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              f(t.name),
              t.tparams.traverse(f(_)),
              f(t.ctor),
              f(t.templ)
            ) mapN DefnF.ClassF[B]

          case t: DefnF.TraitF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              f(t.name),
              t.tparams.traverse(f(_)),
              f(t.ctor),
              f(t.templ)
            ) mapN DefnF.TraitF[B]

          case t: DefnF.ObjectF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              f(t.name),
              f(t.templ)
            ) mapN DefnF.ObjectF[B]

          case t: PkgF[Id @χ] =>
            (
              f(t.ref),
              t.stats.traverse(f(_))
            ) mapN PkgF[B]

          case t: PkgF.ObjectF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              f(t.name),
              f(t.templ)
            ) mapN PkgF.ObjectF[B]

          case t: CtorF.PrimaryF[Id @χ] =>
            (
              t.mods.traverse(f(_)),
              f(t.name),
              t.paramss.traverse(_.traverse(f(_)))
            ) mapN CtorF.PrimaryF[B]

          case t: SelfF[Id @χ] =>
            (
              f(t.name),
              t.decltpe.traverse(f(_))
            ) mapN SelfF[B]


          case t: TemplateF[Id @χ] =>
            (
              t.early.traverse(f(_)),
              t.inits.traverse(f(_)),
              f(t.self),
              t.stats.traverse(f(_))
            ) mapN TemplateF[B]


          case t: SourceF[Id @χ] =>
            t.stats.traverse(f(_)) map SourceF[B]

          case _ =>
            sys.error(s"tree node type ${tree.getClass} is not yet traversable")
        }
      }
  }

}

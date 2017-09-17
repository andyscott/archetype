/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint
package trees

import cats._
import cats.instances.all._
import cats.syntax.all._

import typeclass.TraverseH
import typeclass.syntax._

sealed trait TreeF[+A[_], +I]
object TreeF extends TreeFInstances0

sealed trait RefF[A[_], I <: Ref] extends TreeF[A, I]
sealed trait StatF[A[_], I <: Stat] extends TreeF[A, I]

sealed trait NameF[A[_], I <: Name] extends RefF[A, I]
object NameF {
  final case object AnonymousF extends NameF[Nothing, Name.Anonymous]
  final case class IndeterminateF[A[_]](
    value: String
  ) extends NameF[A, Name.Indeterminate]
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
    mods: List[A[Mod]],
    pats: List[A[Pat]],
    decltpe: Option[A[Type]],
    rhs: A[Term]
  ) extends DefnF[A, Defn.Val]

  // ...
  final case class DefF[A[_]](
    mods   : List[A[Mod]],
    tparams: List[A[Type.Param]],
    paramss: List[List[A[Term.Param]]],
    decltpe: Option[A[Type]],
    body   : A[Term]
  ) extends DefnF[A, Defn.Def] with MemberF.TermF[A, Defn.Def]
  // ...
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
    def traverseH[F[_], A[_], B[_]](f: A ~> (F ∘ B)#λ)(implicit F: Applicative[F]): TreeF[A, ?] ~> (F ∘ TreeF[B, ?])#λ =
      new (TreeF[A, ?] ~> (F ∘ TreeF[B, ?])#λ) {
        def apply[Z](tree: TreeF[A, Z]): F[TreeF[B, Z]] = tree match {

          case t: SourceF[Id] =>
            t.stats.traverse(f(_)).map(SourceF.apply[B])

          case t: TermF.ApplyInfixF[Id] =>
            (f(t.lhs), f(t.op), t.targs.traverse(f(_)), t.args.traverse(f(_)))
              .mapN(TermF.ApplyInfixF.apply[B])

          case t: TermF.NameF => F.pure(t)
          case t: LitF.IntF => F.pure(t)


          case t: DefnF.ValF[Id] =>
            (t.mods.traverse(f(_)), t.pats.traverse(f(_)), t.decltpe.traverse(f(_)), f(t.rhs))
              .mapN(DefnF.ValF.apply[B])

          case t: PatF.VarF[Id] =>
            f(t.name).map(PatF.VarF.apply[B])

          case _ =>
            sys.error(s"tree node type ${tree.getClass} is not yet accounted for")
        }
      }
  }

}

/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint
package apeiron

import cats.Applicative
import cats.Id

trait BoundTypeclasses[W <: World] {

  type FunctionK[F[_], G[_]] = BoundFunctionK[W, F, G]
  type FunctorH[H[_[_], _]]  = BoundFunctorH[W, H]
  type TraverseH[H[_[_], _]] = BoundTraverseH[W, H]

  type ~>[F[_], G[_]]        = FunctionK[F, G]

  // this just lives here for convenience, for the time being
  type ∘[F[_], G[_]] = { type λ[α] = F[G[α]] }
}

trait BoundFunctionK[W <: World, F[_], G[_]] extends WorldBound[W] {
  def apply[A >: ⊥ <: ⊤](fa: F[A]): G[A]
}

trait BoundFunctorH[W <: World, H[_[_], _]] extends BoundTypeclasses[W] with WorldBound[W] {
  final def mapH[F[_], G[_], Z >: ⊥ <: ⊤](hfz: H[F, Z])(f: F ~> G): H[G, Z] = mapH(f)(hfz)
  def mapH[F[_], G[_]](f: F ~> G): H[F, ?] ~> H[G, ?]
}

trait BoundTraverseH[W <: World, H[_[_], _]] extends BoundFunctorH[W, H] {
  def traverseH[F[_]: Applicative, A[_], B[_]](f: A ~> (F ∘ B)#λ): H[A, ?] ~> (F ∘ H[B, ?])#λ

  override def mapH[F[_], G[_]](f: F ~> G): H[F, ?] ~> H[G, ?] = traverseH[Id, F, G](f)
}

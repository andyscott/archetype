/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint
package typeclass

import cats.Applicative
import cats.Id
import cats.~>

package object syntax {
  type ∘[F[_], G[_]] = { type λ[α] = F[G[α]] }
}

import syntax._

trait FunctorH[H[_[_], _]] {
  final def mapH[F[_], G[_], Z](hfz: H[F, Z])(f: F ~> G): H[G, Z] = mapH(f)(hfz)
  def mapH[F[_], G[_]](f: F ~> G): H[F, ?] ~> H[G, ?]
}

trait TraverseH[H[_[_], _]] extends FunctorH[H] {
  final def traverseH[F[_]: Applicative, A[_], B[_], Z](haz: H[A, Z])(f: A ~> (F ∘ B)#λ): F[H[B, Z]] = traverseH(f).apply(haz)
  def traverseH[F[_]: Applicative, A[_], B[_]](f: A ~> (F ∘ B)#λ): H[A, ?] ~> (F ∘ H[B, ?])#λ

  override def mapH[F[_], G[_]](f: F ~> G): H[F, ?] ~> H[G, ?] = traverseH[Id, F, G](f)
}

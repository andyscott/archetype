/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint
package apeiron

import data.HFix

class RecursionBound[W <: World] extends BoundTypeclasses[W] with WorldBound[W] {

  type Algebra  [F[_[_], _], A[_]] = F[A, ?] ~>: A
  type Coalgebra[F[_[_], _], A[_]] = A       ~>: F[A, ?]

  final def hylo[F[_[_], _], A[_], B[_]]
    (alg: Algebra[F, B], coalg: Coalgebra[F, A])
    (implicit F: FunctorH[F])
      : A ~>: B =
    new (A ~>: B) {
      def apply[I >: ⊥ <: ⊤](a: A[I]): B[I] =
        alg(F.mapH(coalg(a))(this))
    }

  trait Based[T[_]] {
    type Base[_[_], _]
  }

  trait Corecursive[T[_]] extends Based[T] { self =>
    def embed: Algebra[Base, T]

    def ana[A[_]]
      (f: Coalgebra[Base, A])
      (implicit BF: FunctorH[Base])
        : A ~>: T =
      hylo(embed, f)
  }

  object Corecursive {
    type Aux[T[_], F[_[_], _]] = Corecursive[T] { type Base[X[_], Y] = F[X, Y] }

    def apply[T[_]](implicit ev: Corecursive[T]): Corecursive.Aux[T, ev.Base] = ev

    implicit def hfixCorecursive[F[_[_], _]]: Corecursive.Aux[HFix[F, ?], F] =
      new Corecursive[HFix[F, ?]] {
        type Base[X[_], Y] = F[X, Y]

        val embed: Algebra[F, HFix[F, ?]] = new Algebra[F, HFix[F, ?]] {
          def apply[A](fa: F[HFix[F, ?], A]): HFix[F, A] = HFix.hfix[F, A](fa)
        }
      }
  }

}

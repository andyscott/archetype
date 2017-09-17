/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint

import cats.~>

import fixedpoint.typeclass.FunctorH

package object recursion {

  type Algebra  [F[_[_], _], A[_]] = F[A, ?] ~> A
  type Coalgebra[F[_[_], _], A[_]] = A       ~> F[A, ?]

  final def hylo[F[_[_], _], A[_], B[_]]
    (alg: Algebra[F, B], coalg: Coalgebra[F, A])
    (implicit F: FunctorH[F])
      : A ~> B = λ[A ~> B](a => alg(F.mapH(coalg(a))(this)))
}

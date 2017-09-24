/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint
package data

final case class HFix[F[_[_], _], A](val unfix: F[HFix[F, ?], A]) extends AnyVal

object HFix {
  def apply[F[_[_], _]]: Fixer[F] = new Fixer[F]

  final class Fixer[F[_[_], _]] private[HFix] {
    def apply[I](unfix: F[HFix[F, ?], I]): HFix[F, I] = HFix[F, I](unfix)
  }
}

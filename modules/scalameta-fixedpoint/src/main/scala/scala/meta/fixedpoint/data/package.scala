/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint

package object data {

  val HFix: HFixModule = HFixImpl
  type HFix[F[_[_], _], A] = HFix.HFix[F, A]

}

/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint.apeiron

object World {
  type Aux[⊥⊥, ⊤⊤ >: ⊥⊥] = World {
    type ⊤ = ⊤⊤
    type ⊥ = ⊥⊥
  }
}

trait WorldBound[W <: World] {
  type ⊤ = W#T
  type ⊥ = W#B
}

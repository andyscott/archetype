/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package fixedpoint

package object apeiron {
  type World = {
    type ⊤ >: ⊥
    type ⊥

    type T = ⊤
    type B = ⊥
  }
}

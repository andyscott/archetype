/*
 * Copyright (C) 2017 Andy Scott
 * Archetype is licensed under the Apache License 2.0
 */

package scala.meta
package adjunct

object parsed extends ParsedSyntax

trait ParsedSyntax {
  implicit def parsedOpsSyntax[T](parsed: Parsed[T]): ParsedOps[T] =
    new ParsedOps[T](parsed)
}

final class ParsedOps[T](val parsed: Parsed[T]) extends AnyVal {

  // https://github.com/scalameta/scalameta/pull/1121

  def fold[A](fe: Parsed.Error => A, ft: T => A): A = parsed match {
    case Parsed.Success(t) => ft(t)
    case e: Parsed.Error => fe(e)
  }

  def toOption: Option[T] = fold(_ => None, t => Some(t))
  def toEither: Either[Parsed.Error, T] = fold(e => Left(e), t => Right(t))

}

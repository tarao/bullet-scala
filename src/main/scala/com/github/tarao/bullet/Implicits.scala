package com.github.tarao
package bullet

/** A trait to allow resolving monads on an implicit conversion. */
trait Implicits {
  implicit val runOnImplicitConversion: Monad.RunOnImplicitConversion =
    new Monad.RunOnImplicitConversion
}

/** Implicits to allow resolving monads on an implicit conversion. */
object Implicits extends Implicits

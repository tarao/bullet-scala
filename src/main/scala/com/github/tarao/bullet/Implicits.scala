package com.github.tarao
package bullet

trait Implicits {
  implicit val runOnImplicitConversion: Monad.RunOnImplicitConversion =
    new Monad.RunOnImplicitConversion
}
object Implicits extends Implicits

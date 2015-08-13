package com.github.tarao
package bullet

import org.scalatest.{FunSpec, Matchers, OptionValues, Inside, Inspectors}

class ExplicitSpec extends FunSpec with Matchers
    with OptionValues with Inside with Inspectors {
  import Example._

  describe("Monad") {
    it("should not run implicitly") {
      val m1 = Monad.Unit(Engine(1001L, 3L))
      assertTypeError("val engine: Option[Engine] = m1")

      val m2 = ResolveEngine(Car(3L, "foo"))
      assertTypeError("val engine: Option[Engine] = m1")

      val m3 = Monad.Unit(Engine(1001L, 3L)).map(identity)
      assertTypeError("val engine: Option[Engine] = m3")

      val m4 = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
        ResolveEngine(Car(3L, "foo"))
      }
      assertTypeError("val engine: Option[Engine] = m4")

      val ms1 = Seq(
        Monad.Unit(Engine(1001L, 3L)),
        Monad.Unit(Engine(1002L, 2L)),
        Monad.Unit(Engine(1003L, 5L))
      )
      assertTypeError("val engines: Seq[Engine] = ms1")

      val ms2 = Seq(
        ResolveEngine(Car(3L, "foo")),
        ResolveEngine(Car(2L, "bar")),
        ResolveEngine(Car(5L, "baz"))
      )
      assertTypeError("val engines: Seq[Engine] = ms2")

      val ms3 = Seq(
        Monad.Unit(Engine(1001L, 3L)),
        Monad.Unit(Engine(1002L, 2L)),
        Monad.Unit(Engine(1003L, 5L))
      ).map { m => m.map(identity) }
      assertTypeError("val engines: Seq[Engine] = ms3")

      val ms4 = Seq(
        Monad.Unit(Engine(1001L, 3L)),
        Monad.Unit(Engine(1002L, 2L)),
        Monad.Unit(Engine(1003L, 5L))
      ).map { m => m.flatMap { e => ResolveEngine(Car(e.carId, "dummy")) } }
      assertTypeError("val engines: Seq[Engine] = ms4")
    }
  }
}

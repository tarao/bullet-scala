package com.github.tarao
package bullet

import org.scalatest.{FunSpec, Matchers, OptionValues, Inside, Inspectors}

class HasASpec extends FunSpec with Matchers
    with OptionValues with Inside with Inspectors {
  import Implicits._
  import Example._

  describe("HasA") {
    import Example.Implicits._

    it("should resolve an engine from a car") {
      val car = Car(3L, "foo")

      // by implicit conversion
      locally {
        val engine1: Option[Engine] = car.toEngine
        engine1 shouldBe Some(Engine(1001L, 3L))

        val engine2: Option[Engine] = for {
          engine <- car.toEngine
        } yield {
          engine shouldBe Engine(1001L, 3L)
          engine
        }
        engine2 shouldBe Some(Engine(1001L, 3L))
      }

      // by explicit run
      locally {
        val engine1 = Monad.run(car.toEngine)
        engine1 shouldBe Some(Engine(1001L, 3L))

        val engine2 = Monad.run(for {
          engine <- car.toEngine
        } yield {
          engine shouldBe Engine(1001L, 3L)
          engine
        })
        engine1 shouldBe Some(Engine(1001L, 3L))
      }
    }

    it("should resolve a crankshaft from a car via an engine") {
      val car = Car(3L, "foo")

      // by implicit conversion
      locally {
        val engine1: Option[Engine] = car.toEngine
        val crankshaft1: Option[Crankshaft] = engine1.flatMap(_.toCrankshaft)
        crankshaft1 shouldBe Some(Crankshaft(1001L))

        val crankshaft2: Option[Crankshaft] =
          car.toEngine.flatMap(_.toCrankshaft)
        crankshaft2 shouldBe Some(Crankshaft(1001L))

        val crankshaft3: Option[Crankshaft] = for {
          engine <- car.toEngine
          crankshaft <- engine.toCrankshaft
        } yield {
          crankshaft shouldBe Crankshaft(1001L)
          crankshaft
        }
        crankshaft3 shouldBe Some(Crankshaft(1001L))
      }

      // by explicit run
      locally {
        val engine1 = Monad.run(car.toEngine)
        val crankshaft1 = engine1.flatMap { e => Monad.run(e.toCrankshaft) }
        crankshaft1 shouldBe Some(Crankshaft(1001L))

        val crankshaft2 = Monad.run(car.toEngine.flatMap(_.toCrankshaft))
        crankshaft2 shouldBe Some(Crankshaft(1001L))

        val crankshaft3 = Monad.run(for {
          engine <- car.toEngine
          crankshaft <- engine.toCrankshaft
        } yield {
          crankshaft shouldBe Crankshaft(1001L)
          crankshaft
        })
        crankshaft3 shouldBe Some(Crankshaft(1001L))
      }
    }

    it("should resolve engines from cars") {
      var cars = Seq((3L, "foo"), (2L, "bar"), (5L, "baz")).map { zipped =>
        (Car.apply _).tupled(zipped)
      }
      val map = Map(
        1001L -> Engine(1001L, 3L),
        1002L -> Engine(1002L, 2L),
        1003L -> Engine(1003L, 5L)
      )

      // by implicit conversion
      locally {
        var engines1: Seq[Engine] = cars.map(_.toEngine)
        engines1 should contain only (
          Engine(1001L, 3L),
          Engine(1003L, 5L)
        )

        val engines2: Seq[Engine] = cars.map { car => for {
          engine <- car.toEngine
        } yield {
          engine shouldBe map(engine.id)
          engine
        } }
        engines2 should contain only (
          Engine(1001L, 3L),
          Engine(1003L, 5L)
        )
      }

      // // by explicit run
      locally {
        var engines1 = Monad.run(cars.map(_.toEngine))
        engines1 should contain only (
          Engine(1001L, 3L),
          Engine(1003L, 5L)
        )

        val engines2 = Monad.run(cars.map { car => for {
          engine <- car.toEngine
        } yield {
          engine shouldBe map(engine.id)
          engine
        } })
        engines2 should contain only (
          Engine(1001L, 3L),
          Engine(1003L, 5L)
        )
      }
    }

    it("should resolve crankshafts from cars via engines") {
      var cars = Seq((3L, "foo"), (2L, "bar"), (5L, "baz")).map { zipped =>
        (Car.apply _).tupled(zipped)
      }

      // by implicit conversion
      locally {
        var engines1: Seq[Engine] = cars.map(_.toEngine)
        var crankshafts1: Seq[Crankshaft] = engines1.map(_.toCrankshaft)
        crankshafts1 should contain only (
          Crankshaft(1001L),
          Crankshaft(1003L)
        )

        val crankshafts2: Seq[Crankshaft] = cars.map { car => for {
          engine <- car.toEngine
          crankshaft <- engine.toCrankshaft
        } yield {
          crankshaft shouldBe Crankshaft(engine.id)
          crankshaft
        } }
        crankshafts2 should contain only (
          Crankshaft(1001L),
          Crankshaft(1003L)
        )
      }

      // by explicit run
      locally {
        var engines1 = Monad.run(cars.map(_.toEngine))
        var crankshafts1 = Monad.run(engines1.map(_.toCrankshaft))
        crankshafts1 should contain only (
          Crankshaft(1001L),
          Crankshaft(1003L)
        )

        val crankshafts2 = Monad.run(cars.map { car => for {
          engine <- car.toEngine
          crankshaft <- engine.toCrankshaft
        } yield {
          crankshaft shouldBe Crankshaft(engine.id)
          crankshaft
        } })
        crankshafts2 should contain only (
          Crankshaft(1001L),
          Crankshaft(1003L)
        )
      }
    }
  }
}

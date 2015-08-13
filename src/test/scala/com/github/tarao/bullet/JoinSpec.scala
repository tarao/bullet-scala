package com.github.tarao
package bullet

import org.scalatest.{FunSpec, Matchers, OptionValues, Inside, Inspectors}

class JoinSpec extends FunSpec with Matchers
    with OptionValues with Inside with Inspectors {
  import Implicits._
  import Example._

  describe("Join") {
    import Example.Implicits._

    it("should join an engine to a car") {
      val car = Car(3L, "foo")

      // by implicit conversion
      locally {
        val car1: Option[CarWithEngine] = car.withEngine
        car1 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L)))

        val car2: Option[CarWithEngine] = for {
          car <- car.withEngine
        } yield {
          car shouldBe (Car(3L, "foo"), Engine(1001L, 3L))
          car
        }
        car2 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L)))
      }

      // by explicit run
      locally {
        val car1 = car.withEngine.run
        car1 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L)))

        val car2 = (for {
          car <- car.withEngine
        } yield {
          car shouldBe (Car(3L, "foo"), Engine(1001L, 3L))
          car
        }).run
        car2 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L)))
      }
    }

    it("should join an engine and wheels to a car") {
      val car = Car(3L, "foo")
      def wheels(carId: CarId): Seq[Wheel] = Seq(
        Wheel(Wheel.frontLeft, carId),
        Wheel(Wheel.frontRight, carId),
        Wheel(Wheel.rearLeft, carId),
        Wheel(Wheel.rearRight, carId)
      )

      // by implicit conversion
      locally {
        val car1: Option[CarWithEngine] = car.withEngine
        val car2: Option[CarWithAll] = car1.flatMap(_.withWheels)
        car2 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L), wheels(3L)))

        val car3: Option[CarWithAll] =
          car.withEngine.flatMap(_.withWheels)
        car3 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L), wheels(3L)))

        val car4: Option[CarWithAll] = for {
          car <- car.withEngine
          car <- car.withWheels
        } yield {
          car shouldBe (Car(3L, "foo"), Engine(1001L, 3L), wheels(3L))
          car
        }
        car4 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L), wheels(3L)))
      }

      // by explicit run
      locally {
        val car1 = car.withEngine.run
        val car2 = car1.flatMap { car => car.withWheels.run }
        car2 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L), wheels(3L)))

        val car3 = car.withEngine.flatMap(_.withWheels).run
        car3 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L), wheels(3L)))

        val car4 = (for {
          car <- car.withEngine
          car <- car.withWheels
        } yield {
          car shouldBe (Car(3L, "foo"), Engine(1001L, 3L), wheels(3L))
          car
        }).run
        car4 shouldBe Some((Car(3L, "foo"), Engine(1001L, 3L), wheels(3L)))
      }
    }

    it("should join engines to cars") {
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
        var cars1: Seq[CarWithEngine] = cars.map(_.withEngine)
        cars1 should contain only (
          (Car(3L, "foo"), Engine(1001L, 3L)),
          (Car(5L, "baz"), Engine(1003L, 5L))
        )

        val cars2: Seq[CarWithEngine] = cars.map { car => for {
          car <- car.withEngine
        } yield {
          car._2 shouldBe map(car._2.id)
          car
        } }
        cars2 should contain only (
          (Car(3L, "foo"), Engine(1001L, 3L)),
          (Car(5L, "baz"), Engine(1003L, 5L))
        )
      }

      // // by explicit run
      locally {
        var cars1 = cars.map(_.withEngine).run
        cars1 should contain only (
          (Car(3L, "foo"), Engine(1001L, 3L)),
          (Car(5L, "baz"), Engine(1003L, 5L))
        )

        val cars2 = cars.map { car => for {
          car <- car.withEngine
        } yield {
          car._2 shouldBe map(car._2.id)
          car
        } }.run
        cars2 should contain only (
          (Car(3L, "foo"), Engine(1001L, 3L)),
          (Car(5L, "baz"), Engine(1003L, 5L))
        )
      }
    }

    it("should join engines and wheels to cars") {
      var cars = Seq((3L, "foo"), (2L, "bar"), (5L, "baz")).map { zipped =>
        (Car.apply _).tupled(zipped)
      }
      var filtered = Seq((3L, "foo"), (5L, "baz")).map { zipped =>
        (Car.apply _).tupled(zipped)
      }
      val engines = Map(
        3L -> Engine(1001L, 3L),
        2L -> Engine(1002L, 2L),
        5L -> Engine(1003L, 5L)
      )
      def wheels(carId: CarId): Seq[Wheel] = Seq(
        Wheel(Wheel.frontLeft, carId),
        Wheel(Wheel.frontRight, carId),
        Wheel(Wheel.rearLeft, carId),
        Wheel(Wheel.rearRight, carId)
      )

      // by implicit conversion
      locally {
        var cars1: Seq[CarWithEngine] = cars.map(_.withEngine)
        var cars2: Seq[CarWithAll] = cars1.map(_.withWheels)
        cars2 should contain only (filtered.map { car =>
          (car, engines(car.id), wheels(car.id))
        }: _*)

        val cars3: Seq[CarWithAll] = cars.map { car => for {
          car <- car.withEngine
          car <- car.withWheels
        } yield(car) }
        cars3 should contain only (filtered.map { car =>
          (car, engines(car.id), wheels(car.id))
        }: _*)
      }

      // by explicit run
      locally {
        var cars1 = cars.map(_.withEngine).run
        var cars2 = cars1.map(_.withWheels).run
        cars2 should contain only (filtered.map { car =>
          (car, engines(car.id), wheels(car.id))
        }: _*)

        val cars3 = cars.map { car => for {
          car <- car.withEngine
          car <- car.withWheels
        } yield(car) }.run
        cars3 should contain only (filtered.map { car =>
          (car, engines(car.id), wheels(car.id))
        }: _*)
      }
    }
  }

  describe("Join with default values") {
    implicit val defaultEngine: Monad.Default[Engine] =
      Monad.Default { Engine(0L, 0L) }

    implicit class CarRelationWithDefault(val car: Car) {
      def withEngine = Join.Monadic(car, CarJoinsAnEngine)
    }

    import Example.Implicits.CarWithEngineRelation

    it("should join an engine to a car with a default value") {
      val car = Car(2L, "bar")

      // by implicit conversion
      locally {
        val car1: Option[CarWithEngine] = car.withEngine
        car1 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L)))

        val car2: Option[CarWithEngine] = for {
          car <- car.withEngine
        } yield {
          car shouldBe (Car(2L, "bar"), Engine(0L, 0L))
          car
        }
        car2 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L)))
      }

      // by explicit run
      locally {
        val car1 = car.withEngine.run
        car1 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L)))

        val car2 = (for {
          car <- car.withEngine
        } yield {
          car shouldBe (Car(2L, "bar"), Engine(0L, 0L))
          car
        }).run
        car2 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L)))
      }
    }

    it("should join an engine and wheels to a car") {
      val car = Car(2L, "bar")
      def wheels(carId: CarId): Seq[Wheel] = Seq(
        Wheel(Wheel.frontLeft, carId),
        Wheel(Wheel.frontRight, carId),
        Wheel(Wheel.rearLeft, carId),
        Wheel(Wheel.rearRight, carId)
      )

      // by implicit conversion
      locally {
        val car1: Option[CarWithEngine] = car.withEngine
        val car2: Option[CarWithAll] = car1.flatMap(_.withWheels)
        car2 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L), wheels(2L)))

        val car3: Option[CarWithAll] =
          car.withEngine.flatMap(_.withWheels)
        car3 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L), wheels(2L)))

        val car4: Option[CarWithAll] = for {
          car <- car.withEngine
          car <- car.withWheels
        } yield {
          car shouldBe (Car(2L, "bar"), Engine(0L, 0L), wheels(2L))
          car
        }
        car4 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L), wheels(2L)))
      }

      // by explicit run
      locally {
        val car1 = car.withEngine.run
        val car2 = car1.flatMap { car => car.withWheels.run }
        car2 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L), wheels(2L)))

        val car3 = car.withEngine.flatMap(_.withWheels).run
        car3 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L), wheels(2L)))

        val car4 = (for {
          car <- car.withEngine
          car <- car.withWheels
        } yield {
          car shouldBe (Car(2L, "bar"), Engine(0L, 0L), wheels(2L))
          car
        }).run
        car4 shouldBe Some((Car(2L, "bar"), Engine(0L, 0L), wheels(2L)))
      }
    }

    it("should join engines to cars") {
      var cars = Seq((3L, "foo"), (2L, "bar"), (5L, "baz")).map { zipped =>
        (Car.apply _).tupled(zipped)
      }
      val map = Map(
        0L -> Engine(0L, 0L),
        1001L -> Engine(1001L, 3L),
        1002L -> Engine(1002L, 2L),
        1003L -> Engine(1003L, 5L)
      )

      // by implicit conversion
      locally {
        var cars1: Seq[CarWithEngine] = cars.map(_.withEngine)
        cars1 should contain only (
          (Car(3L, "foo"), Engine(1001L, 3L)),
          (Car(2L, "bar"), Engine(0L, 0L)),
          (Car(5L, "baz"), Engine(1003L, 5L))
        )

        val cars2: Seq[CarWithEngine] = cars.map { car => for {
          car <- car.withEngine
        } yield {
          car._2 shouldBe map(car._2.id)
          car
        } }
        cars2 should contain only (
          (Car(3L, "foo"), Engine(1001L, 3L)),
          (Car(2L, "bar"), Engine(0L, 0L)),
          (Car(5L, "baz"), Engine(1003L, 5L))
        )
      }

      // // by explicit run
      locally {
        var cars1 = cars.map(_.withEngine).run
        cars1 should contain only (
          (Car(3L, "foo"), Engine(1001L, 3L)),
          (Car(2L, "bar"), Engine(0L, 0L)),
          (Car(5L, "baz"), Engine(1003L, 5L))
        )

        val cars2 = cars.map { car => for {
          car <- car.withEngine
        } yield {
          car._2 shouldBe map(car._2.id)
          car
        } }.run
        cars2 should contain only (
          (Car(3L, "foo"), Engine(1001L, 3L)),
          (Car(2L, "bar"), Engine(0L, 0L)),
          (Car(5L, "baz"), Engine(1003L, 5L))
        )
      }
    }

    it("should join engines and wheels to cars") {
      var cars = Seq((3L, "foo"), (2L, "bar"), (5L, "baz")).map { zipped =>
        (Car.apply _).tupled(zipped)
      }
      var filtered = cars
      val engines = Map(
        3L -> Engine(1001L, 3L),
        2L -> Engine(0L, 0L),
        5L -> Engine(1003L, 5L)
      )
      def wheels(carId: CarId): Seq[Wheel] = Seq(
        Wheel(Wheel.frontLeft, carId),
        Wheel(Wheel.frontRight, carId),
        Wheel(Wheel.rearLeft, carId),
        Wheel(Wheel.rearRight, carId)
      )

      // by implicit conversion
      locally {
        var cars1: Seq[CarWithEngine] = cars.map(_.withEngine)
        var cars2: Seq[CarWithAll] = cars1.map(_.withWheels)
        cars2 should contain only (filtered.map { car =>
          (car, engines(car.id), wheels(car.id))
        }: _*)

        val cars3: Seq[CarWithAll] = cars.map { car => for {
          car <- car.withEngine
          car <- car.withWheels
        } yield(car) }
        cars3 should contain only (filtered.map { car =>
          (car, engines(car.id), wheels(car.id))
        }: _*)
      }

      // by explicit run
      locally {
        var cars1 = cars.map(_.withEngine).run
        var cars2 = cars1.map(_.withWheels).run
        cars2 should contain only (filtered.map { car =>
          (car, engines(car.id), wheels(car.id))
        }: _*)

        val cars3 = cars.map { car => for {
          car <- car.withEngine
          car <- car.withWheels
        } yield(car) }.run
        cars3 should contain only (filtered.map { car =>
          (car, engines(car.id), wheels(car.id))
        }: _*)
      }
    }
  }
}

package com.github.tarao
package bullet

object Example {
  type CarId = Long
  type EngineId = Long
  type WheelPosition = Int
  case class Car(id: CarId, name: String)
  case class Engine(id: EngineId, carId: CarId)
  case class Crankshaft(engineId: EngineId)
  case class Wheel(position: WheelPosition, carId: CarId)
  object Wheel {
    val frontLeft: WheelPosition = 1
    val frontRight: WheelPosition = 2
    val rearLeft: WheelPosition = 3
    val rearRight: WheelPosition = 4
  }
  type CarWithEngine = (Car, Engine)
  type CarWithAll = (Car, Engine, Seq[Wheel])

  class ResolveEngine(car: Car) extends Monad.Resolve[Engine, Car](car) {
    override protected[bullet]
    def run(ms: Seq[Monad.Resolve[Engine, Car]]): Seq[Engine] = {
      ms.map(_.value).map { car =>
        if (car.id % 2 == 0) None
        else Some(Engine(1000L + car.id, car.id))
      }.flatten
    }
  }
  object ResolveEngine {
    def apply(car: Car): Monad.Resolve[Engine, Car] = new ResolveEngine(car)
  }
  class ResolveWheels(car: Car) extends Monad.Resolve[Seq[Wheel], Car](car) {
    override protected[bullet]
    def run(ms: Seq[Monad.Resolve[Seq[Wheel], Car]]): Seq[Seq[Wheel]] = {
      ms.map(_.value).map { car => Seq(
        Wheel(Wheel.frontLeft, car.id),
        Wheel(Wheel.frontRight, car.id),
        Wheel(Wheel.rearLeft, car.id),
        Wheel(Wheel.rearRight, car.id)
      ) }
    }
  }
  object ResolveWheels {
    def apply(car: Car): Monad.Resolve[Seq[Wheel], Car] = new ResolveWheels(car)
  }

  trait CarHasAnEngine extends HasA[Car, Engine] {
    def map(cars: Seq[Car]): Seq[Engine] = {
      var idx: Long = 1000L
      cars.map { car =>
        idx += 1
        if (car.id % 2 == 0) None
        else Some(Engine(idx, car.id))
      }.flatten
    }
  }
  object CarHasAnEngine extends CarHasAnEngine

  trait EngineHasACrankshaft extends HasA[Engine, Crankshaft] {
    def map(engines: Seq[Engine]): Seq[Crankshaft] =
      engines.map { e => Crankshaft(e.id) }
  }
  object EngineHasACrankshaft extends EngineHasACrankshaft

  trait CarJoinsAnEngine extends CarHasAnEngine
      with Join[CarWithEngine, CarId, Car, Engine] {
    def leftKey(car: Car): CarId = car.id
    def rightKey(engine: Engine): CarId = engine.carId
    def merge(car: Car, engine: Engine) = (car, engine)
  }
  object CarJoinsAnEngine extends CarJoinsAnEngine

  trait CarJoinsWheels
      extends Join[CarWithAll, CarId, CarWithEngine, (CarId, Seq[Wheel])] {
    def map(cars: Seq[CarWithEngine]): Seq[(CarId, Seq[Wheel])] =
      cars.map { car =>
        val carId = car._1.id
        (carId, Seq(
          Wheel(Wheel.frontLeft, carId),
          Wheel(Wheel.frontRight, carId),
          Wheel(Wheel.rearLeft, carId),
          Wheel(Wheel.rearRight, carId)
        ))
      }
    def leftKey(car: CarWithEngine): CarId = car._1.id
    def rightKey(wheels: (CarId, Seq[Wheel])): CarId = wheels._1
    def merge(car: CarWithEngine, wheels: (CarId, Seq[Wheel])): CarWithAll =
      (car._1, car._2, wheels._2)
  }
  object CarJoinsWheels extends CarJoinsWheels

  object Implicits {
    implicit class CarRelation(val car: Car) extends AnyVal {
      def toEngine = HasA.Monadic(car, CarHasAnEngine)
      def withEngine = Join.Monadic(car, CarJoinsAnEngine)
    }
    implicit class CarWithEngineRelation(val car: CarWithEngine) extends AnyVal {
      def withWheels = Join.Monadic(car, CarJoinsWheels)
    }
    implicit class EngineRelation(val engine: Engine) extends AnyVal {
      def toCrankshaft = HasA.Monadic(engine, EngineHasACrankshaft)
    }
  }
}

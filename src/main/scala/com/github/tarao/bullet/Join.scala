package com.github.tarao
package bullet

/** Defines an object join of `Left` and `Right` by `Key` resulting `R`.
  *
  * Implementing this interface provides a join of `Left` and `Right`
  * to `R` with respect to `Key`, which can be used in
  * `Join.Monadic[]`.  For instance, to declare that a `Car` has an
  * `Engine` with respect to `CarId` and they can be merged to
  * `CarWithEngine`, implementing `Join[CarWithEngine, Carid, Car,
  * Engine]` and passing it to `Join.Monadic` defines an accessor from
  * a `Car` to a `CarWithEngine`.
  *
  * {{{
  * object HasEngine
  *     extends Join[CarWithEngine, CarId, Car, Engine] {
  *   def leftKey(car: Car): CarId = ???
  *   def rightKey(engine: Engine): CarId = ???
  *   def map(from: Seq[Car]): Seq[Engine] = ???
  *   def merge(car: Car, engine: Engine): CarWithEngine = ???
  * }
  *
  * // An accessor provider
  * case class CarRelation(car: Car) {
  *   def withEngine = Join.Monadic(car, HasEngine)
  * }
  * }}}
  *
  * You need to implement four methods.  First, `leftKey()` and
  * `rightKey()` defines how to resolve a `Key` from a `Left` and a
  * `Right`. Second, `map()` which maps values of `Left` to values of
  * `Right`.  Finally, `merge()` defines how to merge two objects into
  * a value of `R`.
  *
  * Although the defined accessor is for a single instance, the
  * `map`ping defines a list-to-list mapping.  This allows you to
  * define an efficient way to retrieve multiple objects at once.  The
  * resulting list may '''NOT''' be in the same order as the input
  * list or it may lack some elements correspond to those in the
  * input, i.e., the `map`ping is actually a set-to-set mapping and
  * the mapping function may not be total.  If a value lacks a mapping
  * result, that value is excluded from the `merge`d result.  If there
  * are multiple values in the resulting list with the same `Key`
  * (which is resolved by `rightKey`), then a source value with the
  * `Key` will be `merge`d with one of them.
  *
  * In the above example, you may want to have an implicit conversion
  * from a `Car` to an `CarRelation` to allow merging an `Engine` to a
  * `Car`.
  *
  * {{{
  * implicit def carRelation(car: Car): CarRelation = CarRelation(car)
  *
  * // Single object mapping
  * val car: Car = ???
  * val enginedCar: Option[CarWithEngine] = car.withEngine
  *
  * // Multiple object mapping
  * val cars: Seq[Car] = ???
  * val enginedCars: Seq[CarWithEngine] = cars.map(_.withEngine)
  * }}}
  *
  * See the documentation of `Join.Monadic` for the detail of the
  * accessor behavior.
  */
trait Join[R, Key, Left, Right] extends HasA[Left, Right] {
  def leftKey(left: Left): Key
  def rightKey(left: Right): Key
  def merge(left: Left, right: Right): R
  def default: Option[Right] = None
  def withFallback(fallback: Monad.Fallback[Right]): Join[R, Key, Left, Right] =
    new Join.WithFallback(this, fallback)
}
object Join {
  class WithFallback[R, Key, Left, Right](
    join: Join[R, Key, Left, Right],
    fallback: Monad.Fallback[Right]
  ) extends Join[R, Key, Left, Right] {
    def map(from: Seq[Left]): Seq[Right] = join.map(from)
    def leftKey(left: Left): Key = join.leftKey(left)
    def rightKey(right: Right): Key = join.rightKey(right)
    def merge(left: Left, right: Right): R = join.merge(left, right)
    override def default: Option[Right] = fallback.fallback(super.default)
  }

  /** A factory of a monad for joining object field.
    *
    * To initially create a monad instance, use `Join.Monadic.apply()`
    * together with a source object of the mapping and an instance of
    * `Join[]` which describes a field mapping and a way of joining
    * fields.  For instance, a monad for joining a `Car` and an
    * `Engine` into a `CarWithEngine` with respect to `CarId`
    * described by a `Join[CarWithEngine, CarId, Car, Engine]` can be
    * generated as the following.
    *
    * {{{
    * type HasEngine = Join[CarWithEngine, CarId, Car, Engine]
    * val hasEngine: HasEngine = ???
    * val car: Car = ???
    * val m = Join.Monadic(car, hasEngine)
    * }}}
    *
    * A monad can implicitly be converted to an option value of the
    * target type of the join by internally invoking methods of
    * `Join[]` in the conversion.
    *
    * {{{
    * val enginedCar: Option[CarWithEngine] = m
    * }}}
    *
    * Since it is a monad, the value can mapped to another value in a
    * `for` comprehension. (Assume that a `CarWithEngine` has `engine`
    * field of type `Engine` in the next example.)
    *
    * {{{
    * val engine: Option[Engine] = for {
    *   car <- m
    * } yield (car.engine)
    * }}}
    *
    * Note that the `car` in the above example, i.e., the argument of
    * a function passed to `flatMap()` or `map()` of the monad, is a
    * `CarWithEngine`.  You don't need to look inside the option value
    * in this way.  If the resulting option value is `None`, i.e.,
    * there is no target value to join, then the body of the function
    * passed to `flatMap()` or `map()` never be invoked.
    *
    * A list of monadic values can also be converted to a list of
    * the target list type.
    *
    * {{{
    * val cars: Seq[Car] = ???
    * val ms = cars.map { car => Join.Monadic(car, hasEngine) }
    * val enginedCars: Seq[CarWithEngine] = ms
    * }}}
    *
    * In this case, `cars` are mapped to `enginedCars` by methods in
    * `Join[]` all at once.  You can of course use a `for`
    * comprehension for further operations.
    *
    * {{{
    * val engines: Seq[Engine] = ms.map { m => for {
    *   car <- m
    * } yield (car.engine) }
    * }}}
    *
    * If you aim to achieve an efficient object join by converting a
    * list value at once via methods in `Join` --- this is the most
    * common reason for using the monadc object join ---, '''DO NOT'''
    * trigger the implicit conversion inside the loop.
    *
    * {{{
    * val engines: Seq[Engine] = ms.map { m =>
    *   val e: Option[Engine] =
    *     /* !!! methods in Join[] are called here !!! */ for {
    *       car <- m
    *     } yield (car.engine)
    *   e
    * }.flatten
    * }}}
    *
    * It is not difficult to notice the misuse in this way since the
    * object must be recieved as an `Option[Engine]` not an `Engine`
    * to trigger the conversion for each element.
    */
  object Monadic {
    private class Resolve[R, Left, Key, Right](
      left: Left, j: Join[R, Key, Left, Right]
    ) extends Monad.Resolve[R, Left](left) {
      private type This = Monad.Resolve[R, Left]
      override protected[bullet] def run(ms: Seq[This]): Seq[R] = {
        val lefts = ms.map(_.value)
        val toRight: Map[Key, Right] =
          j.map(lefts).map{ r => j.rightKey(r) -> r }(scala.collection.breakOut)
        lefts.map { l =>
          toRight.get(j.leftKey(l)) orElse { j.default } map { j.merge(l, _) }
        }.flatten
      }
    }

    def apply[R, Key, Left, Right](
      value: Left,
      join: Join[R, Key, Left, Right]
    )(implicit fallback: Monad.Fallback[Right]): Monad.Resolve[R, Left] =
      new Resolve[R, Left, Key, Right](
        value,
        if (fallback.hasValue) join.withFallback(fallback) else join
      )
  }
}

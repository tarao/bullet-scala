package com.github.tarao
package bullet

/** Defines an object mapping from `Self` to `Other`.
  *
  * Implementing this interface provides a mapping from `Self` to
  * `Other`, which can be used in `HasA.Monadic[]`.  For instance, to
  * declare that a `Car` has an `Engine`, implementing `HasA[Car,
  * Engine]` and passing it to `HasA.Monadic` defines an accessor from
  * a `Car` to an `Engine`.
  *
  * {{{
  * object HasEngine extends HasA[Car, Engine] {
  *   def map(from: Seq[Car]): Seq[Engine] =
  *     ??? // implement your mapping
  * }
  *
  * // An accessor provider
  * case class CarRelation(car: Car) {
  *   def toEngine = HasA.Monadic(car, HasEngine)
  * }
  * }}}
  *
  * Although the defined accessor is for a single instance, the
  * mapping defines a list-to-list mapping.  This allows you to define
  * an efficient way to retrieve multiple objects at once.  The
  * resulting list may '''NOT''' be in the same order as the input
  * list or it may lack some elements correspond to those in the
  * input, i.e., the mapping is actually a set-to-set mapping and the
  * mapping function may not be total.
  *
  * In the above example, you may want to have an implicit conversion
  * from a `Car` to `CarRelation` to allow accessing an `Engine` via a
  * `Car`.
  *
  * {{{
  * implicit def carRelation(car: Car): CarRelation = CarRelation(car)
  *
  * // Single object mapping
  * val car: Car = ???
  * val engine: Option[Engine] = car.toEngine
  *
  * // Multiple object mapping
  * val cars: Seq[Car] = ???
  * val engines: Seq[Engine] = cars.map(_.toEngine)
  * }}}
  *
  * See the documentation of `HasA.Monadic` for the detail of the
  * accessor behavior.
  */
trait HasA[Self, Other] {
  def map(from: Seq[Self]): Seq[Other]
}
object HasA {
  /** A factory of a monad for a one-to-one object mapping relation.
    *
    * To initially create a monad instance, use `HasA.Monadic.apply()`
    * together with a source object of the mapping and an instance of
    * `HasA[]` which describes a mapping.  For instance, a monad for a
    * mapping from a `Car` to an `Engine` defined by a `HasA[Car,
    * Engine]` can be generated as the following.
    *
    * {{{
    * val hasEngine: HasA[Car, Engine] = ???
    * val car: Car = ???
    * val m = HasA.Monadic(car, hasEngine)
    * }}}
    *
    * A monad can implicitly be converted to an option value of the
    * target type of the mapping by internally invoking `HasA.map()`
    * in the conversion.
    *
    * {{{
    * val engine: Option[Engine] = m
    * }}}
    *
    * Since it is a monad, the value can mapped to another value in a
    * `for` comprehension. (Assume that an `Engine` has `id` field of
    * type `Long` in the next example.)
    *
    * {{{
    * val id: Option[Long] = for { e <- m } yield (e.id)
    * }}}
    *
    * Note that the `e` in the above example, i.e., the argument of a
    * function passed to `flatMap()` or `map()` of the monad, is an
    * `Engine`.  You don't need to look inside the option value in
    * this way.  If the resulting option value is `None`, i.e.,
    * `HasA.map()` does not return a value for the source of the
    * mapping, then the body of the function passed to `flatMap()` or
    * `map()` never be invoked.
    *
    * A list of monadic values can also be converted to a list of
    * the target list type.
    *
    * {{{
    * val cars: Seq[Car] = ???
    * val ms = cars.map { car => HasA.Monadic(car, hasEngine) }
    * val engines: Seq[Engine] = ms
    * }}}
    *
    * In this case, `cars` are mapped to `engines` by `HasA.map()` all
    * at once.  You can of course use a `for` comprehension for
    * further operations.
    *
    * {{{
    * val ids: Seq[Long] = ms.map { m => for { e <- m } yield (e.id) }
    * }}}
    *
    * If you aim to achieve an efficient object mapping by converting
    * a list value at once via `HasA.map()` --- this is the most
    * common reason for using the monadc object mapping ---, '''DO
    * NOT''' trigger the implicit conversion inside the loop.
    *
    * {{{
    * val ids: Seq[Long] = ms.map { m =>
    *   val id: Option[Long] /* !!! HasA.map() is called here !!! */ = for {
    *     e <- m
    *   } yield (e.id)
    *   id
    * }.flatten
    * }}}
    *
    * It is not difficult to notice the misuse in this way since the
    * object must be recieved as an `Option[Long]` not a `Long` to
    * trigger the conversion for each element.
    */
  object Monadic {
    private class Resolve[R, Q](value: Q, rel: HasA[Q, R])
        extends Monad.Resolve[R, Q](value) {
      private type This = Monad.Resolve[R, Q]
      override protected[bullet] def run(ms: Seq[This]): Seq[R] =
        rel.map(ms.map(_.value))
    }
    def apply[R, Q](value: Q, rel: HasA[Q, R]): Monad.Resolve[R, Q] =
      new Resolve(value, rel)
  }
}

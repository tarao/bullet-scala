bullet [![Build Status][travis-img]][travis] [![Coverage status][coverage-img]][coverage] [![Maven Central][maven-img]][maven] [![Scaladoc][javadoc-img]][javadoc]
======

A monadic library to resolve object relations with the aim of avoiding
the N+1 query problem.  The solution requires only pure computations
in Scala depending on neither database implementations nor any other
frameworks.

## Getting started <a name="install"></a>

Add dependency in your `build.sbt` as the following.

```scala
    libraryDependencies ++= Seq(
      "com.github.tarao" %% "bullet" % "0.0.1"
    )
```

The library is available on [Maven Central][maven].  Currently,
supported Scala version is 2.11.

## Overview <a name="overview"></a>

### The problem

Assume that you have `Car` and `Engine` classes and, if `Car` has an
`Engine`, it is resolved by a type class method `toEngine`.

```scala
type CarId = Long
type EngineId = Long

case class Car(id: CarId)
case class Engine(id: EngineId, carId: CarId)

implicit class CarRelation(val car: Car) extends AnyVal {
  def toEngine: Option[Engine] = ...
}
```

It is quite usual to implement `toEngine` method by using a repository
which issues a DB query.

```scala
implicit class CarRelation(val car: Car) extends AnyVal {
  def toEngine: Option[Engine] = EngineRepository.findByCarId(car.id
}

val db = ...
object EngineRepository {
  def findByCarId(carId: CarId): Option[Engine] = db.run {
    sql"SELECT * FROM engine WHERE car_id = $carId LIMIT 1".as[Engine]
  }.headOption
}
```

There is no problem when you resolve an `Engine` from a `Car`.  In
this case, a `SELECT` query is executed internally.

```scala
val car: Car = Car(1234L)
val engine: Option[Engine] = car.toEngine
// SELECT * FROM engine WHERE car_id = 1234 LIMIT 1
```

If you have multiple `Car`s and want to get their `Engine`s, you may
want to write like this.

```scala
val cars: Seq[Car] = Seq(Car(1L), Car(2L), Car(3L), ...)
val engines: Seq[Engine] = cars.map(_.toEngine).flatten
// SELECT * FROM engine WHERE car_id = 1 LIMIT 1
// SELECT * FROM engine WHERE car_id = 2 LIMIT 1
// SELECT * FROM engine WHERE car_id = 3 LIMIT 1
// ...
```

Yes, it works.  But there is a problem that the `SELECT` query is
executed for each `id` of the element of `cars`.  When you have
hundreds or thousands of `cars`, it is likely to be a perfomance
issue.

One way to solve this problem is to `JOIN` tables.  When you
instantiate `Car`s from `car` table in your DB, `engine` table might
also be `INNER JOIN`ed.  This is a quite common solution but not the
best one.  If you have for example `Wheel`s, a `Bumper`, and `Door`s
for a `Car`, you will soon need to `JOIN` them all but not all of them
are needed every time.  You will have to write instantiation methods
with ugly `JOIN` queries for each combination of parts that you need.

Ideally, it would be nice if the last expression executes a single
`SELECT` query.

```scala
val engines: Seq[Engine] = cars.map(_.toEngine).flatten
// SELECT * FROM engine WHERE car_id IN = (1, 2, 3, ...)
```

Is this possible?  In Scala, yes, it is.

### Our solution

All you have to do is to replace `toEngine` method to return an
instance of a monad created by `HasA.Monadic` with an instance of
`HasA[Car, Engine]`, which describes how to resolve `Engine`s from
`Car`s.

```scala
import com.github.tarao.bullet.HasA

implicit class CarRelation(val car: Car) extends AnyVal {
  def toEngine = HasA.Monadic(car, hasEngine)
}
val hasEngine: HasA[Car, Engine] = ...
```

The usage of `toEngine` is quite the same except that (1) you have to
write a type of return value (`Option[Engine]` or `Seq[Engine]` in
this case), (2) you don't need to `flatten`.

```scala
val car: Car = ...
val engine: Option[Engine] = car.toEngine

val cars: Seq[Car] = ...
val engines: Seq[Engine] = cars.map(_.toEngine)
```

The implementation of `HasEngine` should resolve `Engine`s from `Car`s
in a single query.  `HasA[Car, Engine]` has an interface for that
named `map()`, whose type is `Seq[Car] => Seq[Engine]`.  Then the
implementation whould be the following.

```scala
val hasEngine: HasA[Car, Engine] = new HasA[Car, Engine] {
  def map(cars: Seq[Car]): Seq[Engine] = db.run {
    sql"SELECT * FROM engine WHERE car_id IN (${cars.map(_.id)})".as[Engine]
  }
}
```

Note that `map()` method is used for resolving both `Option[Engine]`
and `Seq[Engine]`.  In our example, `toEngine` results in executing a
`SELECT`-`WHERE`-`IN` query in the both cases.

```scala
val car: Car = Car(1234L)
val engine: Option[Engine] = car.toEngine
// SELECT * FROM engine WHERE car_id IN (1234L)

val cars: Seq[Car] = Seq(Car(1L), Car(2L), Car(3L), ...)
val engines: Seq[Engine] = cars.map(_.toEngine)
// SELECT * FROM engine WHERE car_id IN (1, 2, 3, ...)
```

### How does it work?

The key mechanism is a monad, which is a return value of `toEngine`.
In the last example, we receive a monad as a variable of type
`Option[Engine]` or `Seq[Engine]`.  This is actually an implicit
conversion.  If we make it explicit, the example looks like this.

```scala
import com.github.tarao.bullet.Monad

val car: Car = ...
val engine: Option[Engine] = Monad.run(car.toEngine)

val cars: Seq[Car] = ...
val engines: Seq[Engine] = Monad.run(cars.map(_.toEngine))
```

It is `Monad.run()` which actually calls `HasA[].map()`.  Until then,
the invocation of `HasA[].map()` is postponed inside the monad.  If
`Monad.run()` receives multiple monads, it will organize them into an
argument of a single invocation of `HasA[].map()`.  (You may wonder
how it is possible since each monad value has its own instance of
`HasA[]`.  It is actually only the first one in the list to be used if
`Monad.run()` receives multiple monads.)

### Why is it a monad?

The above story does not describe the way using our monad as a monad.
Actually, it is not necessarily a monad as long as it is some kind of
a deferred object.  It is a monad just for convenience.

Let's see an example.  Suppose that an `Engine` has its `Crankshaft`
and there is a type class method `toCrankshaft` defined in the same
way as `toEngine`.  If we don't have the monadic feature, we need to
look up a `Crankshaft` of a `Car` via an `Engine` in the way like
this.

```scala
val car: Car = ...
val engine: Option[Engine] = car.toEngine
val crankshaft: Option[Crankshaft] =
  engine.map(_.toCrankshaft: Option[Crankshaft]).flatten
```

If we use the monadic feature, it can be written like this.

```scala
val car: Car = ...
val crankshaft: Option[Crankshaft] = for {
  e <- car.toEngine
  c <- e.toCrankshaft
} yield(c)
```

This is much easier to read especially when you need a complex
operation on `e` and/or `c`.

## Has-a relation <a name="has-a"></a>

As you have seen in the [overview][], only things you have to do are
to implement `HasA[].map()` and to provide a monad factory using it.

### Summary

- Extend `HasA[From, To]` and implement `map: Seq[From] => Seq[To]`.
- Provide a monad factory which returns `HasA.Monadic(from, hasA)` where:
    - `from` is an instance of `From`
    - `hasA` is an instance of `HasA[From, To]`

## Has-many relation <a name="has-many"></a>

You can specify a list type as `To` type argument of `HasA[From, To]`.
In this case, you can resolve a single value as a `Seq[_]` instead of
`Option[Seq[_]]`, or multiple values as a `Seq[_]` instead of
`Seq[Seq[_]]`.  For example, if you have `HasA[Car, Seq[Wheel]]` and
`toWheels` returns a monad, then these can be used as the following.

```scala
val car: Car = ...
val wheels: Seq[Wheel] = car.toWheels
val cars: Seq[Car] = ...
val totalWheels: Seq[Wheel] = cars.map(_.toWheels)
```

### Summary

- The same as `HasA[From, To]` but `To` is a list type
- The result can be flattened automatically

## Joining related objects <a name="join"></a>

Sometimes you may want to merge related two objects into one.  For
example, if you have `Car`s and their `Engine`s, you may want to have
values of `CarWithEngine`s where those are merged.  To do this, you
can use another interface `Join.Monadic` to define a type class method
`withEngine`.

```scala
import com.github.tarao.bullet.Join

type CarWithEngine = (Car, Engine)

implicit class CarRelation(val car: Car) extends AnyVal {
  def withEngine = Join.Monadic(car, joinEngine)
}

type JoinEngineToCar = Join[CarWithEngine, CarId, Car, Engine]
val joinEngine: JoinEngineToCar = new JoinEngineToCar {
  def map(cars: Seq[Car]): Seq[Engine] = ... // the same as HasA[Car, Engine]
  def leftKey(car: Car): CarId = car.id
  def rightKey(engine: Engine): CarId = engine.carId
  def merge(car: Car, engine: Engine): CarWithEngine = (car, engine)
}
```

This time you have four methods to implement.  `map()`, `leftKey()`,
`rightKey()` and `merge()`.  `map()` is the same as in
`HasA[Car, Engine]`.  `leftKey()` and `rightKey()` provides how you
associate one object to another.  In this case, a `Car` and its
`Engine` should share their `CarId`.  associated objects are passed to
`merge()`.

The usage is quite similar to `toEngine`.

```scala
val car: Car = ...
val enginedCar: Option[CarWithEngine] = car.withEngine

val cars: Seq[Car] = ...
val enginedCars: Seq[CarWithEngine] = cars.map(_.withEngine)
```

### Summary

- Extend `Join[Result, Key, Left, Right]` and implement four methods:
    - `map: Seq[Left] => Seq[Right]`
    - `leftKey: Left => Key`
    - `rightKey: Right => Key`
    - `merge: (Left, Right) => Result`
- Provide a monad factory which returns `Join.Monadic(left, join)` where:
    - `left` is an instance of `Left`
    - `join` is an instance of `Join[Result, Key, Left, Right]`

## Default values <a name="default"></a>

An `Option[]` return value of `Monad.run()` may be `None` if
`HasA[].map()` returns an empty list.  In this case, you can provide a
default value to ensure having some value returned.  This is done by
providing an implicit value of `Monad.Default[]`.  If you provide a
default value, the return value can be received without being wrapped
by `Option[]`.  For example, the following code defines a default
value for an `Engine`.

```scala
implicit val defaultEngine: Monad.Default[Engine] =
  Monad.Default[Engine](Engine(0L, 0L))

val car: Car = ...
val engine: Engine = car.toEngine
```

In this case, note that **the implicit value must be visible in the
scope where the invocation of `Monad.run()` or the implicit conversion
occurs**.

For `Join[]`, you should be careful that you have two choices of types
to provide a default value, either `Result` or `Right` of
`Join[Result, Key, Left, Right]`.  If you provide a default value for
`Result`, then you will always get a value by `Monad.run()` on a
single monad but still get some values lacked by `Monad.run()` on
multiple monads.  You should provide a default value for `Right` to
avoid this.  In this time, **the implicit value must be visible in the
scope where the invoction of `Join.Monadic()` occurs**.

## License <a name="license"></a>

- Copyright (C) INA Lintaro
- MIT License

[travis]: https://travis-ci.org/tarao/bullet-scala
[travis-img]: https://img.shields.io/travis/tarao/bullet-scala.svg?branch=master
[coverage]: https://coveralls.io/github/tarao/bullet-scala?branch=master
[coverage-img]: https://coveralls.io/repos/tarao/bullet-scala/badge.svg?branch=master&service=github
[maven]: https://maven-badges.herokuapp.com/maven-central/com.github.tarao/bullet_2.11
[maven-img]: https://maven-badges.herokuapp.com/maven-central/com.github.tarao/bullet_2.11/badge.svg
[javadoc]: http://javadoc-badge.appspot.com/com.github.tarao/bullet_2.11
[javadoc-img]: http://javadoc-badge.appspot.com/com.github.tarao/bullet_2.11.svg?label=scaladoc

[overview]: #overview

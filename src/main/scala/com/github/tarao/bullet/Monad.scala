package com.github.tarao
package bullet

/** A monad to resolve a collection all together.
  *
  * A monad instance can implicitly be converted into an option value
  * of a resolved object.  A list of monads can implicitly be
  * converted into a list of resolved objects.
  */
sealed trait Monad[R] { def diverge(): Monad.Divergent[R] }
object Monad {
  sealed abstract class Sig[R, Q, N, M](implicit
    monad1: M <:< Monad[Q],
    check1: IsConcreteType[M],
    monad2: N <:< Monad[R],
    check2: IsConcreteType[N]
  ) extends Monad[R] {
    private type This = Sig[R, Q, N, M]
    def map[S](f: R => S): Monad.FlatMapped[S, R, Unit[S], This] =
      Monad.FlatMapped({ (x: R) => Monad.Unit(f(x)) }, this)
    def flatMap[S, Q, N, M](f: R => Sig[S, Q, N, M])(implicit
      monad3: M <:< Monad[Q],
      check3: IsConcreteType[M],
      monad4: N <:< Monad[S],
      check4: IsConcreteType[N]
    ): Monad.FlatMapped[S, R, Sig[S, Q, N, M], This] = Monad.FlatMapped(f, this)
    def diverge(): Divergent[R] = {
      implicit val guard = new RunOnImplicitConversion
      new Divergent({ Monad.run(this) })
    }
  }

  /** A class to create a monad instance from an object of the result type. */
  case class Unit[R](value: R) extends Sig[R, Null, Null, Null] {
    protected[bullet] def run(ms: Seq[Unit[R]]): Seq[R] = ms.map(_.value)
  }

  /** A class to define a resolution from source values to target values.
    *
    * Override `run` to define a concrete resolution.
    */
  abstract case class Resolve[R, Q](value: Q) extends Sig[R, Q, Null, Null] {
    protected[bullet] def run(ms: Seq[Resolve[R, Q]]): Seq[R]
  }

  case class FlatMapped[R, Q, N, M](
    f: Q => N, m: M
  )(implicit
    monad1: M <:< Monad[Q],
    check1: IsConcreteType[M],
    monad2: N <:< Monad[R],
    check2: IsConcreteType[N]
  ) extends Sig[R, Q, N, M] {
    protected[bullet] def run(ms: Seq[FlatMapped[R, Q, N, M]]): Seq[R] = {
      val fs = ms.map(_.f)
      val mapped = Internal.run(ms.map { m => monad1(m.m) })
      Internal.run((fs, mapped).zipped.map { (f, m) => monad2(f(m)) })
    }
  }

  private[Monad] object Internal {
    // These are literally unsafe.  Overall type safety is achieved by
    // assuming that `Seq(m)` and `ms` are of the same type.  This is
    // guaranteed externally by `IsConcreteType[]` and type arguments
    // of `Sig[]`.
    private def run[R](m: Unit[R])(ms: Seq[Monad[R]]): Seq[R] =
      m.run(ms.asInstanceOf[Seq[Unit[R]]])
    private def run[R, Q](m: Resolve[R, Q])(ms: Seq[Monad[R]]): Seq[R] =
      m.run(ms.asInstanceOf[Seq[Resolve[R, Q]]])
    private def run[R, Q, N, M](
      m: FlatMapped[R, Q, N, M]
    )(ms: Seq[Monad[R]]): Seq[R] =
      m.run(ms.asInstanceOf[Seq[FlatMapped[R, Q, N, M]]])
    private[Monad] def run[R](ms: Seq[Monad[R]]): Seq[R] = ms match {
      case (m @ Unit(_))          +: _ => run(m)(ms)
      case (m @ Resolve(_))       +: _ => run(m)(ms)
      case (m @ FlatMapped(_, _)) +: _ => run(m)(ms)
      case Seq()                       => Seq.empty
    }
  }

  import scala.annotation.implicitNotFound

  @implicitNotFound("Invalid monad type: ${M}")
  private[Monad] class IsConcreteType[M](val dummy: Int = 0) extends AnyVal
  private[Monad] object IsConcreteType {
    type This[M] = IsConcreteType[M]
    implicit val none: This[Null] = new This[Null]
    implicit def sig[R, Q, N, M](implicit
      monad1: M <:< Monad[Q],
      check1: IsConcreteType[M],
      monad2: N <:< Monad[R],
      check2: IsConcreteType[N]
    ): This[Sig[R, Q, N, M]] = new This[Sig[R, Q, N, M]]
    implicit def unit[R]: This[Unit[R]] = new This[Unit[R]]
    implicit def resolve[R, Q]: This[Resolve[R, Q]] = new This[Resolve[R, Q]]
    implicit def flatMapped[R, Q, N, M](implicit
      monad1: M <:< Monad[Q],
      check1: IsConcreteType[M],
      monad2: N <:< Monad[R],
      check2: IsConcreteType[N]
    ): This[FlatMapped[R, Q, N, M]] = new This[FlatMapped[R, Q, N, M]]
  }

  import scala.language.implicitConversions

  class RunOnImplicitConversion(val dummy: Int = 0) extends AnyVal

  implicit def run[R, M](ms: Seq[M])(implicit
    guard: RunOnImplicitConversion,
    monad: M <:< Monad[R],
    check: IsConcreteType[M]
  ): Seq[R] = Internal.run(ms.asInstanceOf[Seq[Monad[R]]])

  implicit def run[R, M](m: M)(implicit
    guard: RunOnImplicitConversion,
    monad: M <:< Monad[R],
    check: IsConcreteType[M]
  ): Option[R] = run(Seq(m)).headOption

  implicit def flatten[R, M, T](ms: Seq[M])(implicit
    guard: RunOnImplicitConversion,
    monad: M <:< Monad[T],
    check: IsConcreteType[M],
    seq: T => Seq[R]
  ): Seq[R] = run(ms).flatten

  implicit def flatten[R, M, T](m: M)(implicit
    guard: RunOnImplicitConversion,
    monad: M <:< Monad[T],
    check: IsConcreteType[M],
    seq: T => Seq[R]
  ): Seq[R] = run(Seq(m)).flatten

  class Fallback[T] {
    def hasValue(): Boolean = false
    def fallback(option: Option[T]): Option[T] = option
  }
  object Fallback { implicit def none[T]: Fallback[T] = new Fallback[T] }

  /** A default value provider.
    *
    * If you wish to receive the result of `Monad[]` not in an option
    * value, declare an implicit value of `Default[]` of the result
    * type.  For example, if you wish to receive an `Engine` rather
    * than an `Option[Engine]`, declare an implicit value of
    * `Default[Engine]`.
    * {{{
    * implicit val defaultEngine: Default[Engine] =
    *   Default[Engine] { /* a default value : Engine */ }
    * }}}
    */
  class Default[T](default: () => T) extends Fallback[T] {
    override def hasValue(): Boolean = true
    override def fallback(option: Option[T]): Option[T] =
      option.orElse(Some(default()))
    def apply(option: Option[T]): T = option.getOrElse { default() }
  }
  object Default {
    def apply[T](default: => T): Default[T] =
      new Default[T]({ () => default })
  }

  implicit def runWithDefault[R, M](m: M)(implicit
    guard: RunOnImplicitConversion,
    monad: M <:< Monad[R],
    check: IsConcreteType[M],
    unoption: Default[R]
  ): R = unoption(run(m))

  /** A type class to run each element of `Monad[]`s separately. */
  class Divergent[R](block: => Option[R]) { def run(): Option[R] = block }
  object Divergent {
    import scala.language.implicitConversions
    implicit def fromSeq[R](ms: Seq[Monad[R]]): Seq[Divergent[R]] =
      ms.map(_.diverge)
    implicit def fromMonad[R](m: Monad[R]): Divergent[R] = m.diverge
    def run[R](ds: Seq[Divergent[R]]): Seq[R] = ds.map(_.run).flatten
    def run[R](d: Divergent[R]): Option[R] = d.run
    def flatten[R, S](ds: Seq[Divergent[S]])(implicit
      seq: S => Seq[R]
    ): Seq[R] = run(ds).map(seq(_)).flatten
    def flatten[R, S](d: Divergent[S])(implicit
      seq: S => Seq[R]
    ): Seq[R] = run(Seq(d)).map(seq(_)).flatten
    def runWithDefault[R](d: Divergent[R])(implicit unoption: Default[R]): R =
      unoption(run(d))
  }

  /** A type tag to forbid implicit conversion on a list of monads.
    *
    * It forbids an implicit conversion from
    * `Seq[Monad[SingleValue[T]]]` to `Seq[T]`.  This is useful when
    * you provide no `Resolve.run` which resolves multiple values all
    * together but provide one which resolves each element separately
    * (via `Seq.map` for example) and want to prevent users from
    * expecting that they can be resolved at once.
    */
  case class SingleValue[T](value: T) extends AnyVal
  object SingleValue {
    // $COVERAGE-OFF$
    implicit def runToSingleValueIsForbidden[R, M](m: Seq[M])(implicit
      guard: RunOnImplicitConversion,
      monad: M <:< Monad[SingleValue[R]],
      check: IsConcreteType[M]
    ): Seq[SingleValue[R]] = sys.error("unexpected")
    // $COVERAGE-ON$

    implicit def run[R, M](m: M)(implicit
      guard: RunOnImplicitConversion,
      monad: M <:< Monad[SingleValue[R]],
      check: IsConcreteType[M]
    ): Option[R] = Monad.run(m).map(_.value)

    implicit def flatten[R, M, T](m: M)(implicit
      guard: RunOnImplicitConversion,
      monad: M <:< Monad[SingleValue[T]],
      check: IsConcreteType[M],
      seq: T => Seq[R]
    ): Seq[R] = Monad.run(Seq(m)).map(_.value).flatten

    implicit def runWithDefault[R, M](m: M)(implicit
      guard: RunOnImplicitConversion,
      monad: M <:< Monad[SingleValue[R]],
      check: IsConcreteType[M],
      unoption: Default[R]
    ): R = unoption(SingleValue.run(m))
  }
}

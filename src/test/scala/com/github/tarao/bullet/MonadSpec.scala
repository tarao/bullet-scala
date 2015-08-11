package com.github.tarao
package bullet

import org.scalatest.{FunSpec, Matchers, OptionValues, Inside, Inspectors}

class MonadSpec extends FunSpec with Matchers
    with OptionValues with Inside with Inspectors {
  import Example._

  describe("Monad") {
    it("should resolve") {
      locally { // Unit
        val engine: Option[Engine] = Monad.Unit(Engine(1001L, 3L))
        engine shouldBe Some(Engine(1001L, 3L))

        val ms1: Seq[Monad.Unit[Engine]] = Seq(Monad.Unit(Engine(1001L, 3L)))
        val engines1: Seq[Engine] = ms1
        engines1 should contain only Engine(1001L, 3L)

        val ms2 = Array(Monad.Unit(Engine(1001L, 3L))).toSeq
        val engines2: Seq[Engine] = ms2
        engines2 shouldBe Array(Engine(1001L, 3L))

        val ms3 = Array(Monad.Unit(Engine(1001L, 3L)))
        val engines3: Seq[Engine] = Monad.run(ms3)
        engines3 shouldBe Array(Engine(1001L, 3L))
      }

      locally { // Resolve
        val engine1: Option[Engine] = ResolveEngine(Car(3L, "foo"))
        engine1 shouldBe Some(Engine(1003L, 3L))

        val engine2: Option[Engine] = ResolveEngine(Car(2L, "bar"))
        engine2 shouldBe None

        val ms1 = Seq(
          ResolveEngine(Car(3L, "foo")),
          ResolveEngine(Car(2L, "bar")),
          ResolveEngine(Car(5L, "baz"))
        )
        val engines1: Seq[Engine] = ms1
        engines1 should contain only (Engine(1003L, 3L), Engine(1005L, 5L))

        val ms2 = Array(
          ResolveEngine(Car(3L, "foo")),
          ResolveEngine(Car(2L, "bar")),
          ResolveEngine(Car(5L, "baz"))
        ).toSeq
        val engines2: Seq[Engine] = ms2
        engines2 shouldBe Array(Engine(1003L, 3L), Engine(1005L, 5L))

        val ms3 = Array(
          ResolveEngine(Car(3L, "foo")),
          ResolveEngine(Car(2L, "bar")),
          ResolveEngine(Car(5L, "baz"))
        )
        val engines3: Seq[Engine] = Monad.run(ms3)
        engines3 shouldBe Array(Engine(1003L, 3L), Engine(1005L, 5L))
      }
    }

    it("can be mapped") {
      val engine1: Option[Engine] = Monad.Unit(Engine(1001L, 3L)).map { e =>
        Engine(1002L, 5L)
      }
      engine1 shouldBe Some(Engine(1002L, 5L))

      val engine2: Option[Engine] = for {
        e <- Monad.Unit(Engine(1001L, 3L))
      } yield(Engine(1002L, 5L))
      engine2 shouldBe Some(Engine(1002L, 5L))

      val engine3: Option[Engine] = ResolveEngine(Car(3L, "foo")).map { e =>
        Engine(1002L, 5L)
      }
      engine3 shouldBe Some(Engine(1002L, 5L))

      val engine4: Option[Engine] = for {
        e <- ResolveEngine(Car(3L, "foo"))
      } yield(Engine(1002L, 5L))
      engine4 shouldBe Some(Engine(1002L, 5L))

      val engine5: Option[Engine] = ResolveEngine(Car(2L, "bar")).map { e =>
        Engine(1002L, 5L)
      }
      engine5 shouldBe None

      val engine6: Option[Engine] = for {
        e <- ResolveEngine(Car(2L, "bar"))
      } yield(Engine(1002L, 5L))
      engine6 shouldBe None

      val ms1 = Seq(Monad.Unit(Engine(1001L, 3L)).map { e =>
        Engine(1002L, 5L)
      })
      val engines1: Seq[Engine] = ms1
      engines1 shouldBe Seq(Engine(1002L, 5L))

      val ms2 = Seq(for {
        e <- Monad.Unit(Engine(1001L, 3L))
      } yield(Engine(1002L, 5L)))
      val engines2: Seq[Engine] = ms2
      engines2 shouldBe Seq(Engine(1002L, 5L))

      val ms3 = Seq(
        ResolveEngine(Car(3L, "foo")),
        ResolveEngine(Car(2L, "bar")),
        ResolveEngine(Car(5L, "baz"))
      ).map { m => m.map { e => Engine(e.id + 1, e.carId + 1) } }
      val engines3: Seq[Engine] = ms3
      engines3 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))

      val ms4 = Seq(
        Car(3L, "foo"),
        Car(2L, "bar"),
        Car(5L, "baz")
      ).map { car => for {
        e <- ResolveEngine(car)
      } yield(Engine(e.id + 1, e.carId + 1)) }
      val engines4: Seq[Engine] = ms4
      engines4 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
    }

    it("can be flat-mapped") {
      val engine1: Option[Engine] = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
        Monad.Unit(Engine(1002L, 5L))
      }
      engine1 shouldBe Some(Engine(1002L, 5L))

      val engine2: Option[Engine] = for {
        e <- Monad.Unit(Engine(1001L, 3L))
        e <- Monad.Unit(Engine(1002L, 5L))
      } yield(e)
      engine2 shouldBe Some(Engine(1002L, 5L))

      val engine3: Option[Engine] = ResolveEngine(Car(3L, "foo")).flatMap { e =>
        Monad.Unit(Engine(1002L, 5L))
      }
      engine3 shouldBe Some(Engine(1002L, 5L))

      val engine4: Option[Engine] = for {
        e <- ResolveEngine(Car(3L, "foo"))
        e <- Monad.Unit(Engine(1002L, 5L))
      } yield(e)
      engine4 shouldBe Some(Engine(1002L, 5L))

      val engine5: Option[Engine] = ResolveEngine(Car(3L, "foo")).flatMap { e =>
        ResolveEngine(Car(5L, "baz"))
      }
      engine5 shouldBe Some(Engine(1005L, 5L))

      val engine6: Option[Engine] = for {
        e <- ResolveEngine(Car(3L, "foo"))
        e <- ResolveEngine(Car(5L, "baz"))
      } yield(e)
      engine6 shouldBe Some(Engine(1005L, 5L))

      val engine7: Option[Engine] = ResolveEngine(Car(3L, "foo")).flatMap { e =>
        ResolveEngine(Car(2L, "bar"))
      }
      engine7 shouldBe None

      val engine8: Option[Engine] = for {
        e <- ResolveEngine(Car(3L, "foo"))
        e <- ResolveEngine(Car(2L, "bar"))
      } yield(e)
      engine8 shouldBe None

      val ms1 = Seq(Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
        Monad.Unit(Engine(1002L, 5L))
      })
      val engines1: Seq[Engine] = ms1
      engines1 shouldBe Seq(Engine(1002L, 5L))

      val ms2 = Seq(for {
        e <- Monad.Unit(Engine(1001L, 3L))
        e <- Monad.Unit(Engine(1002L, 5L))
      } yield(e))
      val engines2: Seq[Engine] = ms2
      engines2 shouldBe Seq(Engine(1002L, 5L))

      val ms3 = Seq(
        ResolveEngine(Car(3L, "foo")),
        ResolveEngine(Car(2L, "bar")),
        ResolveEngine(Car(5L, "baz"))
      ).map(_.flatMap { e => Monad.Unit(Engine(e.id + 1, e.carId + 1)) })
      val engines3: Seq[Engine] = ms3
      engines3 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))

      val ms4 = Seq(
        Car(3L, "foo"),
        Car(2L, "bar"),
        Car(5L, "baz")
      ).map { car => for {
        e <- ResolveEngine(car)
        e <- Monad.Unit(Engine(e.id + 1, e.carId + 1))
      } yield(e) }
      val engines4: Seq[Engine] = ms4
      engines4 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))

      val ms5 = Seq(
        ResolveEngine(Car(3L, "foo")),
        ResolveEngine(Car(2L, "bar")),
        ResolveEngine(Car(5L, "baz"))
      ).map(_.flatMap { e => ResolveEngine(Car(e.carId+2, "dummy")) })
      val engines5: Seq[Engine] = ms5
      engines5 shouldBe Array(Engine(1005L, 5L), Engine(1007L, 7L))

      val ms6 = Seq(
        Car(3L, "foo"),
        Car(2L, "bar"),
        Car(5L, "baz")
      ).map { car => for {
        e <- ResolveEngine(car)
        e <- ResolveEngine(Car(e.carId+2, "dummy"))
      } yield(e) }
      val engines6: Seq[Engine] = ms6
      engines6 shouldBe Array(Engine(1005L, 5L), Engine(1007L, 7L))
    }

    it("may resolve to an empty list") {
      val ms1: Seq[Monad.Unit[Engine]] = Seq.empty
      val engines1: Seq[Engine] = ms1
      engines1 shouldBe empty

      val ms2 = ms1.map { m => m.map { e => Engine(1002L, 5L) } }
      val engines2: Seq[Engine] = ms2
      engines2 shouldBe empty

      val ms3 = ms1.map { m => for (e <- m) yield(Engine(1002L, 5L)) }
      val engines3: Seq[Engine] = ms3
      engines3 shouldBe empty

      val ms4 = ms1.map { m => m.flatMap {
        e => Monad.Unit(Engine(1002L, 5L))
      } }
      val engines4: Seq[Engine] = ms4
      engines4 shouldBe empty

      val ms5 = ms1.map { m => for {
        e <- m
        e <- Monad.Unit(Engine(1002L, 5L))
      } yield(e) }
      val engines5: Seq[Engine] = ms5
      engines5 shouldBe empty

      val ms6 = ms1.map { m => m.flatMap {
        e => ResolveEngine(Car(3L, "foo"))
      } }
      val engines6: Seq[Engine] = ms6
      engines6 shouldBe empty

      val ms7 = ms1.map { m => for {
        e <- m
        e <- ResolveEngine(Car(3L, "foo"))
      } yield(e) }
      val engines7: Seq[Engine] = ms7
      engines7 shouldBe empty

      val ms8 = Seq(Car(2L, "bar")).map(ResolveEngine(_))
      val engines8: Seq[Engine] = ms8
      engines8 shouldBe empty

      val ms9 = ms8.map { m => m.map { e => Engine(1002L, 5L) } }
      val engines9: Seq[Engine] = ms9
      engines9 shouldBe empty

      val ms10 = ms8.map { m => for (e <- m) yield(Engine(1002L, 5L)) }
      val engines10: Seq[Engine] = ms10
      engines10 shouldBe empty

      val ms11 = ms8.map { m => m.flatMap {
        e => Monad.Unit(Engine(1002L, 5L))
      } }
      val engines11: Seq[Engine] = ms11
      engines11 shouldBe empty

      val ms12 = ms8.map { m => for {
        e <- m
        e <- Monad.Unit(Engine(1002L, 5L))
      } yield(e) }
      val engines12: Seq[Engine] = ms12
      engines12 shouldBe empty

      val ms13 = ms8.map { m => m.flatMap {
        e => ResolveEngine(Car(3L, "foo"))
      } }
      val engines13: Seq[Engine] = ms13
      engines13 shouldBe empty

      val ms14 = ms8.map { m => for {
        e <- m
        e <- ResolveEngine(Car(3L, "foo"))
      } yield(e) }
      val engines14: Seq[Engine] = ms14
      engines14 shouldBe empty
    }

    it("should resolve with flattening opitons in a list") {
      locally {
        val ms1 = Monad.Unit(Seq(Engine(1001L, 3L)))
        val engines1: Option[Seq[Engine]] = ms1
        engines1 shouldBe Some(Seq(Engine(1001L, 3L)))

        val engines2: Seq[Engine] = ms1
        engines2 should contain only Engine(1001L, 3L)
      }

      locally {
        val ms1 = ResolveWheels(Car(3L, "foo"))
        val wheels1: Option[Seq[Wheel]] = ms1
        wheels1 shouldBe Some(Seq(
          Wheel(1, 3L),
          Wheel(2, 3L),
          Wheel(3, 3L),
          Wheel(4, 3L)
        ))

        val wheels2: Seq[Wheel] = ms1
        wheels2 should contain only (
          Wheel(1, 3L),
          Wheel(2, 3L),
          Wheel(3, 3L),
          Wheel(4, 3L)
        )

        val wheels3: Option[Seq[Wheel]] =
          ResolveWheels(Car(3L, "foo")).map(_.map { wheel =>
            Wheel(wheel.position, wheel.carId + 1)
          })
        wheels3 shouldBe Some(Seq(
          Wheel(1, 4L),
          Wheel(2, 4L),
          Wheel(3, 4L),
          Wheel(4, 4L)
        ))

        val wheels4: Seq[Wheel] =
          ResolveWheels(Car(3L, "foo")).map(_.map { wheel =>
            Wheel(wheel.position, wheel.carId + 1)
          })
        wheels4 should contain only (
          Wheel(1, 4L),
          Wheel(2, 4L),
          Wheel(3, 4L),
          Wheel(4, 4L)
        )
      }
    }

    it("should resolve with flattening a nested list") {
      locally {
        val ms1 = Seq(Monad.Unit(Seq(Engine(1001L, 3L))))
        val engines1: Seq[Seq[Engine]] = ms1
        engines1 should contain only Seq(Engine(1001L, 3L))

        val engines2: Seq[Engine] = ms1
        engines2 should contain only Engine(1001L, 3L)

        val ms3 = Array(Monad.Unit(Array(Engine(1001L, 3L)))).toSeq
        val engines3: Seq[Array[Engine]] = ms3
        engines3 should contain only Array(Engine(1001L, 3L))

        val engines4: Seq[Engine] = ms3
        engines4 should contain only Engine(1001L, 3L)
      }

      locally {
        val ms1 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => ResolveWheels(car) }
        val wheels1: Seq[Seq[Wheel]] = ms1
        wheels1 should contain only (
          Seq(Wheel(1, 3L), Wheel(2, 3L), Wheel(3, 3L), Wheel(4, 3L)),
          Seq(Wheel(1, 2L), Wheel(2, 2L), Wheel(3, 2L), Wheel(4, 2L)),
          Seq(Wheel(1, 5L), Wheel(2, 5L), Wheel(3, 5L), Wheel(4, 5L))
        )

        val wheels2: Seq[Wheel] = ms1
        wheels2 should contain only (
          Wheel(1, 3L), Wheel(2, 3L), Wheel(3, 3L), Wheel(4, 3L),
          Wheel(1, 2L), Wheel(2, 2L), Wheel(3, 2L), Wheel(4, 2L),
          Wheel(1, 5L), Wheel(2, 5L), Wheel(3, 5L), Wheel(4, 5L)
        )

        val wheels3: Seq[Seq[Wheel]] = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => ResolveWheels(car).map(_.map { wheel =>
          Wheel(wheel.position, wheel.carId + 1)
        }) }
        wheels3 should contain only (
          Seq(Wheel(1, 4L), Wheel(2, 4L), Wheel(3, 4L), Wheel(4, 4L)),
          Seq(Wheel(1, 3L), Wheel(2, 3L), Wheel(3, 3L), Wheel(4, 3L)),
          Seq(Wheel(1, 6L), Wheel(2, 6L), Wheel(3, 6L), Wheel(4, 6L))
        )

        val wheels4: Seq[Wheel] = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => ResolveWheels(car).map(_.map { wheel =>
          Wheel(wheel.position, wheel.carId + 1)
        }) }
        wheels4 should contain only (
          Wheel(1, 4L), Wheel(2, 4L), Wheel(3, 4L), Wheel(4, 4L),
          Wheel(1, 3L), Wheel(2, 3L), Wheel(3, 3L), Wheel(4, 3L),
          Wheel(1, 6L), Wheel(2, 6L), Wheel(3, 6L), Wheel(4, 6L)
        )
      }
    }

    it("should resolve with a default value") {
      implicit val default: Monad.Default[Engine] =
        Monad.Default(Engine(0L, 0L))

      locally {
        val car = Car(3L, "foo")
        val engine = Engine(1001L, 3L)

        val engine1: Option[Engine] = Monad.Unit(engine)
        engine1 shouldBe Some(engine)

        val engine2: Engine = Monad.Unit(engine)
        engine2 shouldBe engine

        val engine3: Option[Engine] =
          Monad.Unit(car).flatMap { _ => Monad.Unit(engine) }
        engine3 shouldBe Some(engine)

        val engine4: Engine = Monad.Unit(car).flatMap { _ =>
          Monad.Unit(engine)
        }
        engine4 shouldBe engine

        val engine5: Option[Engine] = Monad.Unit(car).flatMap { _ =>
          Monad.Unit(engine)
        }.map(identity)
        engine5 shouldBe Some(engine)

        val engine6: Engine = Monad.Unit(car).flatMap { _ =>
          Monad.Unit(engine)
        }.map(identity)
        engine6 shouldBe engine

        val engine7: Option[Engine] = for {
          c <- Monad.Unit(car)
          e <- Monad.Unit(engine)
        } yield(e)
        engine7 shouldBe Some(engine)

        val engine8: Engine = for {
          c <- Monad.Unit(car)
          e <- Monad.Unit(engine)
        } yield(e)
        engine8 shouldBe engine
      }

      locally {
        val engine1: Option[Engine] = ResolveEngine(Car(3L, "foo"))
        engine1 shouldBe Some(Engine(1003L, 3L))

        val engine2: Engine = ResolveEngine(Car(3L, "foo"))
        engine2 shouldBe Engine(1003L, 3L)

        val engine3: Engine = ResolveEngine(Car(2L, "bar"))
        engine3 shouldBe Engine(0L, 0L)

        val engine4: Option[Engine] =
          ResolveEngine(Car(3L, "foo")).flatMap { e => Monad.Unit(e) }
        engine4 shouldBe Some(Engine(1003L, 3L))

        val engine5: Engine = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          Monad.Unit(e)
        }
        engine5 shouldBe Engine(1003L, 3L)

        val engine6: Engine = ResolveEngine(Car(2L, "bar")).flatMap { e =>
          Monad.Unit(e)
        }
        engine6 shouldBe Engine(0L, 0L)

        val engine7: Option[Engine] =
          ResolveEngine(Car(3L, "foo")).flatMap { e =>
            Monad.Unit(e)
          }.map(identity)
        engine7 shouldBe Some(Engine(1003L, 3L))

        val engine8: Engine = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          Monad.Unit(e)
        }.map(identity)
        engine8 shouldBe Engine(1003L, 3L)

        val engine9: Engine = ResolveEngine(Car(2L, "bar")).flatMap { e =>
          Monad.Unit(e)
        }.map(identity)
        engine9 shouldBe Engine(0L, 0L)

        val engine10: Option[Engine] = for {
          e <- ResolveEngine(Car(3L, "foo"))
          e <- Monad.Unit(e)
        } yield(e)
        engine10 shouldBe Some(Engine(1003L, 3L))

        val engine11: Engine = for {
          e <- ResolveEngine(Car(3L, "foo"))
          e <- Monad.Unit(e)
        } yield(e)
        engine11 shouldBe Engine(1003L, 3L)

        val engine12: Engine = for {
          e <- ResolveEngine(Car(2L, "bar"))
          e <- Monad.Unit(e)
        } yield(e)
        engine12 shouldBe Engine(0L, 0L)
      }
    }

    it("should resolve to a single value") {
      val engine1: Option[Engine] =
        Monad.Unit(Engine(1001L, 3L)).map { e => Monad.SingleValue(e) }
      engine1 shouldBe Some(Engine(1001L, 3L))

      val engine2: Option[Engine] =
        ResolveEngine(Car(3L, "foo")).map { e => Monad.SingleValue(e) }
      engine2 shouldBe Some(Engine(1003L, 3L))

      val engine3: Seq[Engine] =
        Monad.Unit(Engine(1001L, 3L)).map { e => Monad.SingleValue(Seq(e)) }
      engine3 shouldBe Seq(Engine(1001L, 3L))

      val engine4: Seq[Engine] =
        ResolveEngine(Car(3L, "foo")).map { e => Monad.SingleValue(Seq(e)) }
      engine4 shouldBe Seq(Engine(1003L, 3L))

      implicit val default: Monad.Default[Engine] =
        Monad.Default(Engine(0L, 0L))

      val engine5: Engine =
        ResolveEngine(Car(2L, "bar")).map { e => Monad.SingleValue(e) }
      engine5 shouldBe Engine(0L, 0L)

      val ms1 = Seq(Monad.Unit(Engine(1001L, 3L)).map { e =>
        Monad.SingleValue(e)
      })
      assertTypeError("val engines: Seq[Engine] = ms1")

      val ms2 = Seq(ResolveEngine(Car(3L, "foo")).map { e =>
        Monad.SingleValue(e)
      })
      assertTypeError("val engines: Seq[Engine] = ms2")
    }

    it("should reject invalid monad type") {
      val car = Car(3L, "foo")

      locally {
        val m = ResolveEngine(car)
        type T = Monad.Sig[Engine, Null, Null, Null]
        assertTypeError("val n: T = m")
      }

      locally {
        val m1 = ResolveEngine(car).flatMap { _ =>
          Monad.Unit(Crankshaft(1001L))
        }
        val m2 = Monad.Unit(Engine(0L, 0L)).flatMap { _ =>
          Monad.Unit(Crankshaft(1003L))
        }

        type T1 = Monad.FlatMapped[Crankshaft, Engine, Null, Nothing]
        type T2 = Monad.FlatMapped[Crankshaft, Engine, Null, Null]
        assertTypeError("val m: T1 = m1")
        assertTypeError("val m: T1 = m2")
        assertTypeError("val m: T2 = m1")
        assertTypeError("val m: T2 = m2")
      }
    }

    it("should reject inconsistent translation") {
      val car = Car(3L, "foo")
      val m1 = ResolveEngine(car).flatMap { _ =>
        Monad.Unit(Crankshaft(1001L))
      }
      val m2 = Monad.Unit(Engine(0L, 0L)).flatMap { _ =>
        Monad.Unit(Crankshaft(1003L))
      }
      val seq1: Seq[Monad[Crankshaft]] = Seq(m1, m2)
      assertTypeError("val r: Seq[Crankshaft] = seq1")
      assertTypeError("val r: Seq[Crankshaft] = Monad.run(seq1)")
      val seq2 = Seq(m1, m2)
      assertTypeError("val r: Seq[Crankshaft] = seq2")
      assertTypeError("val r: Seq[Crankshaft] = Monad.run(seq2)")

      val m3: Monad.FlatMapped[Crankshaft, Engine, Monad.Sig[Crankshaft, Null, Null, Null], _] = m1
      val m4:  Monad.FlatMapped[Crankshaft, Engine, Monad.Sig[Crankshaft, Null, Null, Null], _] = m2
      val seq3: Seq[ Monad.FlatMapped[Crankshaft, Engine, Monad.Sig[Crankshaft, Null, Null, Null], _]] = Seq(m3, m4)
      assertTypeError("val r: Seq[Crankshaft] = seq3")
      assertTypeError("val r: Seq[Crankshaft] = Monad.run(seq3)")

      val m5 = Monad.Unit(Car(3L, "foo")).flatMap { c =>
        Monad.Unit(Engine(0L, 0L))
      }
      val m6 = Monad.Unit(Car(3L, "foo")).flatMap(ResolveEngine(_))
      val seq4 = Seq(m5, m6)
      assertTypeError("val r: Seq[Engine] = seq4")
      assertTypeError("val r: Seq[Engine] = Monad.run(seq4)")

      val m7 = Monad.Unit(Car(3L, "foo")).flatMap { c => m5 }
      val m8 = Monad.Unit(Car(3L, "foo")).flatMap { c => m6 }
      val seq5 = Seq(m7, m8)
      assertTypeError("val r: Seq[Engine] = seq5")
      assertTypeError("val r: Seq[Engine] = Monad.run(seq5)")
    }

    it("should satisfy monad laws") {
      // m map identity = m

      locally {
        val m = Monad.Unit(Engine(1001L, 3L))
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m.map(identity)
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo"))
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m.map(identity)
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m.map(identity)
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m.map(identity)
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m.map(identity)
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m.map(identity)
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m.map(identity)
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m.map(identity)
        e1 shouldBe e2
      }

      // m map g map f = m map { x => f(g(x)) }

      val f = (e: Engine) => Engine(e.id, e.carId*2)
      val g = (e: Engine) => Engine(e.id, e.carId+2)

      locally {
        val m = Monad.Unit(Engine(1001L, 3L))
        val e1: Option[Engine] = m map { e => f(g(e)) }
        val e2: Option[Engine] = m map g map f
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo"))
        val e1: Option[Engine] = m map { e => f(g(e)) }
        val e2: Option[Engine] = m map g map f
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m map { e => f(g(e)) }
        val e2: Option[Engine] = m map g map f
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m map { e => f(g(e)) }
        val e2: Option[Engine] = m map g map f
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m map { e => f(g(e)) }
        val e2: Option[Engine] = m map g map f
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m map { e => f(g(e)) }
        val e2: Option[Engine] = m map g map f
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m map { e => f(g(e)) }
        val e2: Option[Engine] = m map g map f
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m map { e => f(g(e)) }
        val e2: Option[Engine] = m map g map f
        e1 shouldBe e2
      }

      // m map f = m flatMap { x => unit(f(x)) }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L))
        val e1: Option[Engine] = m map f
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(f(x)) }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo"))
        val e1: Option[Engine] = m map f
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(f(x)) }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m map f
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(f(x)) }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m map f
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(f(x)) }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m map f
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(f(x)) }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m map f
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(f(x)) }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m map f
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(f(x)) }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m map f
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(f(x)) }
        e1 shouldBe e2
      }

      // m flatMap unit = m

      locally {
        val m = Monad.Unit(Engine(1001L, 3L))
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(x) }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo"))
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(x) }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(x) }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(x) }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(x) }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(x) }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(x) }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m
        val e2: Option[Engine] = m flatMap { x => Monad.Unit(x) }
        e1 shouldBe e2
      }

      // unit(x) flatMap f = f(x)

      locally {
        val f = (e: Engine) => Monad.Unit(e)
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      locally {
        val f = (e: Engine) => ResolveEngine(Car(3L, "foo"))
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      locally {
        val f = (e: Engine) => Monad.Unit(e).map { _ => e }
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      locally {
        val f = (e: Engine) => ResolveEngine(Car(3L, "foo")).map { _ => e }
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      locally {
        val f = (e: Engine) => Monad.Unit(e).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      locally {
        val f = (e: Engine) => ResolveEngine(Car(3L, "foo")).flatMap { _ =>
          Monad.Unit(e)
        }
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      locally {
        val f = (e: Engine) => Monad.Unit(e).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      locally {
        val f = (e: Engine) => ResolveEngine(Car(3L, "foo")).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      locally {
        val f = (e: Engine) => Monad.Unit(g(e))
        val x = Engine(1001L, 3L)
        val e1: Option[Engine] = f(x)
        val e2: Option[Engine] = Monad.Unit(x) flatMap f
        e1 shouldBe e2
      }

      // m flatMap g flatMap f = m flatMap { x => g(x) flatMap f }

      val ff = (e: Engine) => Monad.Unit(f(e))
      val gg = (e: Engine) => Monad.Unit(g(e))

      locally {
        val m = Monad.Unit(Engine(1001L, 3L))
        val e1: Option[Engine] = m flatMap gg flatMap ff
        val e2: Option[Engine] = m flatMap { x => gg(x) flatMap ff }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo"))
        val e1: Option[Engine] = m flatMap gg flatMap ff
        val e2: Option[Engine] = m flatMap { x => gg(x) flatMap ff }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m flatMap gg flatMap ff
        val e2: Option[Engine] = m flatMap { x => gg(x) flatMap ff }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).map { e => Engine(1002L, 5L) }
        val e1: Option[Engine] = m flatMap gg flatMap ff
        val e2: Option[Engine] = m flatMap { x => gg(x) flatMap ff }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m flatMap gg flatMap ff
        val e2: Option[Engine] = m flatMap { x => gg(x) flatMap ff }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        }
        val e1: Option[Engine] = m flatMap gg flatMap ff
        val e2: Option[Engine] = m flatMap { x => gg(x) flatMap ff }
        e1 shouldBe e2
      }

      locally {
        val m = Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m flatMap gg flatMap ff
        val e2: Option[Engine] = m flatMap { x => gg(x) flatMap ff }
        e1 shouldBe e2
      }

      locally {
        val m = ResolveEngine(Car(3L, "foo")).flatMap { e =>
          ResolveEngine(Car(2L, "bar"))
        }
        val e1: Option[Engine] = m flatMap gg flatMap ff
        val e2: Option[Engine] = m flatMap { x => gg(x) flatMap ff }
        e1 shouldBe e2
      }
    }
  }

  class Counter {
    var count = 0
    def run[T](block: => T): T = {
      count += 1
      block
    }
  }
  class CountedResolveEngine(car: Car, counter: Counter)
      extends ResolveEngine(car) {
    override protected[bullet]
    def run(ms: Seq[Monad.Resolve[Engine, Car]]): Seq[Engine] =
      counter.run { super.run(ms) }
  }
  object CountedResolveEngine {
    def apply(car: Car, counter: Counter): Monad.Resolve[Engine, Car] =
      new CountedResolveEngine(car, counter)
  }

  describe("Resolution") {
    it("should occur once for a list") {
      locally {
        val c = new Counter
        val engine1: Option[Engine] = CountedResolveEngine(Car(3L, "foo"), c)
        engine1 shouldBe Some(Engine(1003L, 3L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine2: Option[Engine] = CountedResolveEngine(Car(2L, "bar"), c)
        engine2 shouldBe None
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms1 = Seq(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        )
        val engines1: Seq[Engine] = ms1
        engines1 should contain only (Engine(1003L, 3L), Engine(1005L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms2 = Array(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        ).toSeq
        val engines2: Seq[Engine] = ms2
        engines2 shouldBe Array(Engine(1003L, 3L), Engine(1005L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms3 = Array(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        )
        val engines3: Seq[Engine] = Monad.run(ms3)
        engines3 shouldBe Array(Engine(1003L, 3L), Engine(1005L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine3: Option[Engine] =
          CountedResolveEngine(Car(3L, "foo"), c).map { e =>
            Engine(1002L, 5L)
          }
        engine3 shouldBe Some(Engine(1002L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine4: Option[Engine] = for {
          e <- CountedResolveEngine(Car(3L, "foo"), c)
        } yield(Engine(1002L, 5L))
        engine4 shouldBe Some(Engine(1002L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine5: Option[Engine] =
          CountedResolveEngine(Car(2L, "bar"), c).map { e =>
            Engine(1002L, 5L)
          }
        engine5 shouldBe None
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine6: Option[Engine] = for {
          e <- CountedResolveEngine(Car(2L, "bar"), c)
        } yield(Engine(1002L, 5L))
        engine6 shouldBe None
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms3 = Seq(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        ).map { m => m.map { e => Engine(e.id + 1, e.carId + 1) } }
        val engines3: Seq[Engine] = ms3
        engines3 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms4 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- CountedResolveEngine(car, c)
        } yield(Engine(e.id + 1, e.carId + 1)) }
        val engines4: Seq[Engine] = ms4
        engines4 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine3: Option[Engine] =
          CountedResolveEngine(Car(3L, "foo"), c).flatMap { e =>
            Monad.Unit(Engine(1002L, 5L))
          }
        engine3 shouldBe Some(Engine(1002L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine4: Option[Engine] = for {
          e <- CountedResolveEngine(Car(3L, "foo"), c)
          e <- Monad.Unit(Engine(1002L, 5L))
        } yield(e)
        engine4 shouldBe Some(Engine(1002L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine5: Option[Engine] =
          CountedResolveEngine(Car(3L, "foo"), c).flatMap { e =>
            CountedResolveEngine(Car(5L, "baz"), c)
          }
        engine5 shouldBe Some(Engine(1005L, 5L))
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val engine6: Option[Engine] = for {
          e <- CountedResolveEngine(Car(3L, "foo"), c)
          e <- CountedResolveEngine(Car(5L, "baz"), c)
        } yield(e)
        engine6 shouldBe Some(Engine(1005L, 5L))
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val engine7: Option[Engine] =
          CountedResolveEngine(Car(3L, "foo"), c).flatMap { e =>
            CountedResolveEngine(Car(2L, "bar"), c)
          }
        engine7 shouldBe None
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val engine8: Option[Engine] = for {
          e <- CountedResolveEngine(Car(3L, "foo"), c)
          e <- CountedResolveEngine(Car(2L, "bar"), c)
        } yield(e)
        engine8 shouldBe None
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val ms3 = Seq(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        ).map(_.flatMap { e => Monad.Unit(Engine(e.id + 1, e.carId + 1)) })
        val engines3: Seq[Engine] = ms3
        engines3 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms4 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- CountedResolveEngine(car, c)
          e <- Monad.Unit(Engine(e.id + 1, e.carId + 1))
        } yield(e) }
        val engines4: Seq[Engine] = ms4
        engines4 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms5 = Seq(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        ).map(_.flatMap { e =>
          CountedResolveEngine(Car(e.carId+2, "dummy"), c)
        })
        val engines5: Seq[Engine] = ms5
        engines5 shouldBe Array(Engine(1005L, 5L), Engine(1007L, 7L))
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val ms6 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- CountedResolveEngine(car, c)
          e <- CountedResolveEngine(Car(e.carId+2, "dummy"), c)
        } yield(e) }
        val engines6: Seq[Engine] = ms6
        engines6 shouldBe Array(Engine(1005L, 5L), Engine(1007L, 7L))
        c.count shouldBe 2
      }

      val ms1: Seq[Monad.Unit[Engine]] = Seq.empty

      locally {
        val c = new Counter
        val ms6 = ms1.map { m => m.flatMap {
          e => CountedResolveEngine(Car(3L, "foo"), c)
        } }
        val engines6: Seq[Engine] = ms6
        engines6 shouldBe empty
        c.count shouldBe 0
      }

      locally {
        val c = new Counter
        val ms7 = ms1.map { m => for {
          e <- m
          e <- CountedResolveEngine(Car(3L, "foo"), c)
        } yield(e) }
        val engines7: Seq[Engine] = ms7
        engines7 shouldBe empty
        c.count shouldBe 0
      }

      locally {
        val c = new Counter
        val ms8 = Seq(Car(2L, "bar")).map(CountedResolveEngine(_, c))
        val engines8: Seq[Engine] = ms8
        engines8 shouldBe empty
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms8 = Seq(Car(2L, "bar")).map(CountedResolveEngine(_, c))
        val ms13 = ms8.map { m => m.flatMap {
          e => CountedResolveEngine(Car(3L, "foo"), c)
        } }
        val engines13: Seq[Engine] = ms13
        engines13 shouldBe empty
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms8 = Seq(Car(2L, "bar")).map(CountedResolveEngine(_, c))
        val ms14 = ms8.map { m => for {
          e <- m
          e <- CountedResolveEngine(Car(3L, "foo"), c)
        } yield(e) }
        val engines14: Seq[Engine] = ms14
        engines14 shouldBe empty
        c.count shouldBe 1
      }
    }
  }

  describe("Monad.Divergent") {
    it("should run monads") {
      locally { // Unit
        val engine: Option[Engine] = Monad.Unit(Engine(1001L, 3L))
        engine shouldBe Some(Engine(1001L, 3L))

        val ms1: Seq[Monad.Unit[Engine]] = Seq(Monad.Unit(Engine(1001L, 3L)))
        val engines1: Seq[Engine] = Monad.Divergent.run(ms1)
        engines1 should contain only Engine(1001L, 3L)

        val ms2 = Array(Monad.Unit(Engine(1001L, 3L))).toSeq
        val engines2: Seq[Engine] = Monad.Divergent.run(ms2)
        engines2 shouldBe Array(Engine(1001L, 3L))
      }

      locally { // Resolve
        val engine1: Option[Engine] =
          Monad.Divergent.run(ResolveEngine(Car(3L, "foo")))
        engine1 shouldBe Some(Engine(1003L, 3L))

        val engine2: Option[Engine] =
          Monad.Divergent.run(ResolveEngine(Car(2L, "bar")))
        engine2 shouldBe None

        val ms1 = Seq(
          ResolveEngine(Car(3L, "foo")),
          ResolveEngine(Car(2L, "bar")),
          ResolveEngine(Car(5L, "baz"))
        )
        val engines1: Seq[Engine] = Monad.Divergent.run(ms1)
        engines1 should contain only (Engine(1003L, 3L), Engine(1005L, 5L))

        val ms2 = Array(
          ResolveEngine(Car(3L, "foo")),
          ResolveEngine(Car(2L, "bar")),
          ResolveEngine(Car(5L, "baz"))
        ).toSeq
        val engines2: Seq[Engine] = Monad.Divergent.run(ms2)
        engines2 shouldBe Array(Engine(1003L, 3L), Engine(1005L, 5L))
      }

      locally { // mapped
        val engine1: Option[Engine] =
          Monad.Divergent.run(Monad.Unit(Engine(1001L, 3L)).map { e =>
            Engine(1002L, 5L)
          })
        engine1 shouldBe Some(Engine(1002L, 5L))

        val engine2: Option[Engine] = Monad.Divergent.run(for {
          e <- Monad.Unit(Engine(1001L, 3L))
        } yield(Engine(1002L, 5L)))
        engine2 shouldBe Some(Engine(1002L, 5L))

        val engine3: Option[Engine] =
          Monad.Divergent.run(ResolveEngine(Car(3L, "foo")).map { e =>
            Engine(1002L, 5L)
          })
        engine3 shouldBe Some(Engine(1002L, 5L))

        val engine4: Option[Engine] = Monad.Divergent.run(for {
          e <- ResolveEngine(Car(3L, "foo"))
        } yield(Engine(1002L, 5L)))
        engine4 shouldBe Some(Engine(1002L, 5L))

        val engine5: Option[Engine] =
          Monad.Divergent.run(ResolveEngine(Car(2L, "bar")).map { e =>
            Engine(1002L, 5L)
          })
        engine5 shouldBe None

        val engine6: Option[Engine] = Monad.Divergent.run(for {
          e <- ResolveEngine(Car(2L, "bar"))
        } yield(Engine(1002L, 5L)))
        engine6 shouldBe None

        val ms1 = Seq(Monad.Unit(Engine(1001L, 3L)).map { e =>
          Engine(1002L, 5L)
        })
        val engines1: Seq[Engine] = Monad.Divergent.run(ms1)
        engines1 shouldBe Seq(Engine(1002L, 5L))

        val ms2 = Seq(for {
          e <- Monad.Unit(Engine(1001L, 3L))
        } yield(Engine(1002L, 5L)))
        val engines2: Seq[Engine] = Monad.Divergent.run(ms2)
        engines2 shouldBe Seq(Engine(1002L, 5L))

        val ms3 = Seq(
          ResolveEngine(Car(3L, "foo")),
          ResolveEngine(Car(2L, "bar")),
          ResolveEngine(Car(5L, "baz"))
        ).map { m => m.map { e => Engine(e.id + 1, e.carId + 1) } }
        val engines3: Seq[Engine] = Monad.Divergent.run(ms3)
        engines3 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))

        val ms4 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- ResolveEngine(car)
        } yield(Engine(e.id + 1, e.carId + 1)) }
        val engines4: Seq[Engine] = Monad.Divergent.run(ms4)
        engines4 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
      }

      locally { // flat-mapped
        val engine1: Option[Engine] =
          Monad.Divergent.run(Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
            Monad.Unit(Engine(1002L, 5L))
          })
        engine1 shouldBe Some(Engine(1002L, 5L))

        val engine2: Option[Engine] = Monad.Divergent.run(for {
          e <- Monad.Unit(Engine(1001L, 3L))
          e <- Monad.Unit(Engine(1002L, 5L))
        } yield(e))
        engine2 shouldBe Some(Engine(1002L, 5L))

        val engine3: Option[Engine] =
          Monad.Divergent.run(ResolveEngine(Car(3L, "foo")).flatMap { e =>
            Monad.Unit(Engine(1002L, 5L))
          })
        engine3 shouldBe Some(Engine(1002L, 5L))

        val engine4: Option[Engine] = Monad.Divergent.run(for {
          e <- ResolveEngine(Car(3L, "foo"))
          e <- Monad.Unit(Engine(1002L, 5L))
        } yield(e))
        engine4 shouldBe Some(Engine(1002L, 5L))

        val engine5: Option[Engine] =
          Monad.Divergent.run(ResolveEngine(Car(3L, "foo")).flatMap { e =>
            ResolveEngine(Car(5L, "baz"))
          })
        engine5 shouldBe Some(Engine(1005L, 5L))

        val engine6: Option[Engine] = Monad.Divergent.run(for {
          e <- ResolveEngine(Car(3L, "foo"))
          e <- ResolveEngine(Car(5L, "baz"))
        } yield(e))
        engine6 shouldBe Some(Engine(1005L, 5L))

        val engine7: Option[Engine] =
          Monad.Divergent.run(ResolveEngine(Car(3L, "foo")).flatMap { e =>
            ResolveEngine(Car(2L, "bar"))
          })
        engine7 shouldBe None

        val engine8: Option[Engine] = Monad.Divergent.run(for {
          e <- ResolveEngine(Car(3L, "foo"))
          e <- ResolveEngine(Car(2L, "bar"))
        } yield(e))
        engine8 shouldBe None

        val ms1 = Seq(Monad.Unit(Engine(1001L, 3L)).flatMap { e =>
          Monad.Unit(Engine(1002L, 5L))
        })
        val engines1: Seq[Engine] = Monad.Divergent.run(ms1)
        engines1 shouldBe Seq(Engine(1002L, 5L))

        val ms2 = Seq(for {
          e <- Monad.Unit(Engine(1001L, 3L))
          e <- Monad.Unit(Engine(1002L, 5L))
        } yield(e))
        val engines2: Seq[Engine] = Monad.Divergent.run(ms2)
        engines2 shouldBe Seq(Engine(1002L, 5L))

        val ms3 = Seq(
          ResolveEngine(Car(3L, "foo")),
          ResolveEngine(Car(2L, "bar")),
          ResolveEngine(Car(5L, "baz"))
        ).map(_.flatMap { e => Monad.Unit(Engine(e.id + 1, e.carId + 1)) })
        val engines3: Seq[Engine] = Monad.Divergent.run(ms3)
        engines3 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))

        val ms4 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- ResolveEngine(car)
          e <- Monad.Unit(Engine(e.id + 1, e.carId + 1))
        } yield(e) }
        val engines4: Seq[Engine] = Monad.Divergent.run(ms4)
        engines4 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))

        val ms5 = Seq(
          ResolveEngine(Car(3L, "foo")),
          ResolveEngine(Car(2L, "bar")),
          ResolveEngine(Car(5L, "baz"))
        ).map(_.flatMap { e => ResolveEngine(Car(e.carId+2, "dummy")) })
        val engines5: Seq[Engine] = Monad.Divergent.run(ms5)
        engines5 shouldBe Array(Engine(1005L, 5L), Engine(1007L, 7L))

        val ms6 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- ResolveEngine(car)
          e <- ResolveEngine(Car(e.carId+2, "dummy"))
        } yield(e) }
        val engines6: Seq[Engine] = Monad.Divergent.run(ms6)
        engines6 shouldBe Array(Engine(1005L, 5L), Engine(1007L, 7L))
      }

      locally { // empty list
        val ms1: Seq[Monad.Unit[Engine]] = Seq.empty
        val engines1: Seq[Engine] = Monad.Divergent.run(ms1)
        engines1 shouldBe empty

        val ms2 = ms1.map { m => m.map { e => Engine(1002L, 5L) } }
        val engines2: Seq[Engine] = Monad.Divergent.run(ms2)
        engines2 shouldBe empty

        val ms3 = ms1.map { m => for (e <- m) yield(Engine(1002L, 5L)) }
        val engines3: Seq[Engine] = Monad.Divergent.run(ms3)
        engines3 shouldBe empty

        val ms4 = ms1.map { m => m.flatMap {
          e => Monad.Unit(Engine(1002L, 5L))
        } }
        val engines4: Seq[Engine] = Monad.Divergent.run(ms4)
        engines4 shouldBe empty

        val ms5 = ms1.map { m => for {
          e <- m
          e <- Monad.Unit(Engine(1002L, 5L))
        } yield(e) }
        val engines5: Seq[Engine] = Monad.Divergent.run(ms5)
        engines5 shouldBe empty

        val ms6 = ms1.map { m => m.flatMap {
          e => ResolveEngine(Car(3L, "foo"))
        } }
        val engines6: Seq[Engine] = Monad.Divergent.run(ms6)
        engines6 shouldBe empty

        val ms7 = ms1.map { m => for {
          e <- m
          e <- ResolveEngine(Car(3L, "foo"))
        } yield(e) }
        val engines7: Seq[Engine] = Monad.Divergent.run(ms7)
        engines7 shouldBe empty

        val ms8 = Seq(Car(2L, "bar")).map(ResolveEngine(_))
        val engines8: Seq[Engine] = Monad.Divergent.run(ms8)
        engines8 shouldBe empty

        val ms9 = ms8.map { m => m.map { e => Engine(1002L, 5L) } }
        val engines9: Seq[Engine] = Monad.Divergent.run(ms9)
        engines9 shouldBe empty

        val ms10 = ms8.map { m => for (e <- m) yield(Engine(1002L, 5L)) }
        val engines10: Seq[Engine] = Monad.Divergent.run(ms10)
        engines10 shouldBe empty

        val ms11 = ms8.map { m => m.flatMap {
          e => Monad.Unit(Engine(1002L, 5L))
        } }
        val engines11: Seq[Engine] = Monad.Divergent.run(ms11)
        engines11 shouldBe empty

        val ms12 = ms8.map { m => for {
          e <- m
          e <- Monad.Unit(Engine(1002L, 5L))
        } yield(e) }
        val engines12: Seq[Engine] = Monad.Divergent.run(ms12)
        engines12 shouldBe empty

        val ms13 = ms8.map { m => m.flatMap {
          e => ResolveEngine(Car(3L, "foo"))
        } }
        val engines13: Seq[Engine] = Monad.Divergent.run(ms13)
        engines13 shouldBe empty

        val ms14 = ms8.map { m => for {
          e <- m
          e <- ResolveEngine(Car(3L, "foo"))
        } yield(e) }
        val engines14: Seq[Engine] = Monad.Divergent.run(ms14)
        engines14 shouldBe empty
      }

      locally { // flatten option lists
        locally {
          val ms1 = Monad.Unit(Seq(Engine(1001L, 3L)))
          val engines1: Option[Seq[Engine]] = Monad.Divergent.run(ms1)
          engines1 shouldBe Some(Seq(Engine(1001L, 3L)))

          val engines2: Seq[Engine] = Monad.Divergent.flatten(ms1)
          engines2 should contain only Engine(1001L, 3L)
        }

        locally {
          val ms1 = ResolveWheels(Car(3L, "foo"))
          val wheels1: Option[Seq[Wheel]] = Monad.Divergent.run(ms1)
          wheels1 shouldBe Some(Seq(
            Wheel(1, 3L),
            Wheel(2, 3L),
            Wheel(3, 3L),
            Wheel(4, 3L)
          ))

          val wheels2: Seq[Wheel] = Monad.Divergent.flatten(ms1)
          wheels2 should contain only (
            Wheel(1, 3L),
            Wheel(2, 3L),
            Wheel(3, 3L),
            Wheel(4, 3L)
          )

          val wheels3: Option[Seq[Wheel]] =
            Monad.Divergent.run(
              ResolveWheels(Car(3L, "foo")).map(_.map { wheel =>
                Wheel(wheel.position, wheel.carId + 1)
              }))
          wheels3 shouldBe Some(Seq(
            Wheel(1, 4L),
            Wheel(2, 4L),
            Wheel(3, 4L),
            Wheel(4, 4L)
          ))

          val wheels4: Seq[Wheel] =
            Monad.Divergent.flatten(
              ResolveWheels(Car(3L, "foo")).map(_.map { wheel =>
                Wheel(wheel.position, wheel.carId + 1)
              }))
          wheels4 should contain only (
            Wheel(1, 4L),
            Wheel(2, 4L),
            Wheel(3, 4L),
            Wheel(4, 4L)
          )
        }
      }

      locally { // flatten nested list
        locally {
          val ms1 = Seq(Monad.Unit(Seq(Engine(1001L, 3L))))
          val engines1: Seq[Seq[Engine]] = Monad.Divergent.run(ms1)
          engines1 should contain only Seq(Engine(1001L, 3L))

          val engines2: Seq[Engine] = Monad.Divergent.flatten(ms1)
          engines2 should contain only Engine(1001L, 3L)

          val ms3 = Array(Monad.Unit(Array(Engine(1001L, 3L)))).toSeq
          val engines3: Seq[Array[Engine]] = Monad.Divergent.run(ms3)
          engines3 should contain only Array(Engine(1001L, 3L))

          val engines4: Seq[Engine] = Monad.Divergent.flatten(ms3)
          engines4 should contain only Engine(1001L, 3L)
        }

        locally {
          val ms1 = Seq(
            Car(3L, "foo"),
            Car(2L, "bar"),
            Car(5L, "baz")
          ).map { car => ResolveWheels(car) }
          val wheels1: Seq[Seq[Wheel]] = Monad.Divergent.run(ms1)
          wheels1 should contain only (
            Seq(Wheel(1, 3L), Wheel(2, 3L), Wheel(3, 3L), Wheel(4, 3L)),
            Seq(Wheel(1, 2L), Wheel(2, 2L), Wheel(3, 2L), Wheel(4, 2L)),
            Seq(Wheel(1, 5L), Wheel(2, 5L), Wheel(3, 5L), Wheel(4, 5L))
          )

          val wheels2: Seq[Wheel] = Monad.Divergent.flatten(ms1)
          wheels2 should contain only (
            Wheel(1, 3L), Wheel(2, 3L), Wheel(3, 3L), Wheel(4, 3L),
            Wheel(1, 2L), Wheel(2, 2L), Wheel(3, 2L), Wheel(4, 2L),
            Wheel(1, 5L), Wheel(2, 5L), Wheel(3, 5L), Wheel(4, 5L)
          )

          val wheels3: Seq[Seq[Wheel]] = Monad.Divergent.run(Seq(
            Car(3L, "foo"),
            Car(2L, "bar"),
            Car(5L, "baz")
          ).map { car => ResolveWheels(car).map(_.map { wheel =>
            Wheel(wheel.position, wheel.carId + 1)
          }) })
          wheels3 should contain only (
            Seq(Wheel(1, 4L), Wheel(2, 4L), Wheel(3, 4L), Wheel(4, 4L)),
            Seq(Wheel(1, 3L), Wheel(2, 3L), Wheel(3, 3L), Wheel(4, 3L)),
            Seq(Wheel(1, 6L), Wheel(2, 6L), Wheel(3, 6L), Wheel(4, 6L))
          )

          val wheels4: Seq[Wheel] = Monad.Divergent.flatten(Seq(
            Car(3L, "foo"),
            Car(2L, "bar"),
            Car(5L, "baz")
          ).map { car => ResolveWheels(car).map(_.map { wheel =>
            Wheel(wheel.position, wheel.carId + 1)
          }) })
          wheels4 should contain only (
            Wheel(1, 4L), Wheel(2, 4L), Wheel(3, 4L), Wheel(4, 4L),
            Wheel(1, 3L), Wheel(2, 3L), Wheel(3, 3L), Wheel(4, 3L),
            Wheel(1, 6L), Wheel(2, 6L), Wheel(3, 6L), Wheel(4, 6L)
          )
        }
      }

      locally { // default value
        implicit val default: Monad.Default[Engine] =
          Monad.Default(Engine(0L, 0L))

        locally {
          val car = Car(3L, "foo")
          val engine = Engine(1001L, 3L)

          val engine1: Option[Engine] = Monad.Divergent.run(Monad.Unit(engine))
          engine1 shouldBe Some(engine)

          val engine2: Engine =
            Monad.Divergent.runWithDefault(Monad.Unit(engine))
          engine2 shouldBe engine

          val engine3: Option[Engine] =
            Monad.Divergent.run(Monad.Unit(car).flatMap { _ =>
              Monad.Unit(engine)
            })
          engine3 shouldBe Some(engine)

          val engine4: Engine =
            Monad.Divergent.runWithDefault(Monad.Unit(car).flatMap { _ =>
              Monad.Unit(engine)
            })
          engine4 shouldBe engine

          val engine5: Option[Engine] =
            Monad.Divergent.run(Monad.Unit(car).flatMap { _ =>
              Monad.Unit(engine)
            }.map(identity))
          engine5 shouldBe Some(engine)

          val engine6: Engine =
            Monad.Divergent.runWithDefault(Monad.Unit(car).flatMap { _ =>
              Monad.Unit(engine)
            }.map(identity))
          engine6 shouldBe engine

          val engine7: Option[Engine] = Monad.Divergent.run(for {
            c <- Monad.Unit(car)
            e <- Monad.Unit(engine)
          } yield(e))
          engine7 shouldBe Some(engine)

          val engine8: Engine = Monad.Divergent.runWithDefault(for {
            c <- Monad.Unit(car)
            e <- Monad.Unit(engine)
          } yield(e))
          engine8 shouldBe engine
        }

        locally {
          val engine1: Option[Engine] =
            Monad.Divergent.run(ResolveEngine(Car(3L, "foo")))
          engine1 shouldBe Some(Engine(1003L, 3L))

          val engine2: Engine =
            Monad.Divergent.runWithDefault(ResolveEngine(Car(3L, "foo")))
          engine2 shouldBe Engine(1003L, 3L)

          val engine3: Engine =
            Monad.Divergent.runWithDefault(ResolveEngine(Car(2L, "bar")))
          engine3 shouldBe Engine(0L, 0L)

          val engine4: Option[Engine] =
            Monad.Divergent.run(ResolveEngine(Car(3L, "foo")).flatMap { e =>
              Monad.Unit(e)
            })
          engine4 shouldBe Some(Engine(1003L, 3L))

          val engine5: Engine =
            Monad.Divergent.runWithDefault(ResolveEngine(Car(3L, "foo")).flatMap { e =>
              Monad.Unit(e)
            })
          engine5 shouldBe Engine(1003L, 3L)

          val engine6: Engine =
            Monad.Divergent.runWithDefault(ResolveEngine(Car(2L, "bar")).flatMap { e =>
              Monad.Unit(e)
            })
          engine6 shouldBe Engine(0L, 0L)

          val engine7: Option[Engine] =
            Monad.Divergent.run(ResolveEngine(Car(3L, "foo")).flatMap { e =>
              Monad.Unit(e)
            }.map(identity))
          engine7 shouldBe Some(Engine(1003L, 3L))

          val engine8: Engine =
            Monad.Divergent.runWithDefault(ResolveEngine(Car(3L, "foo")).flatMap { e =>
              Monad.Unit(e)
            }.map(identity))
          engine8 shouldBe Engine(1003L, 3L)

          val engine9: Engine =
            Monad.Divergent.runWithDefault(ResolveEngine(Car(2L, "bar")).flatMap { e =>
              Monad.Unit(e)
            }.map(identity))
          engine9 shouldBe Engine(0L, 0L)

          val engine10: Option[Engine] = Monad.Divergent.run(for {
            e <- ResolveEngine(Car(3L, "foo"))
            e <- Monad.Unit(e)
          } yield(e))
          engine10 shouldBe Some(Engine(1003L, 3L))

          val engine11: Engine = Monad.Divergent.runWithDefault(for {
            e <- ResolveEngine(Car(3L, "foo"))
            e <- Monad.Unit(e)
          } yield(e))
          engine11 shouldBe Engine(1003L, 3L)

          val engine12: Engine = Monad.Divergent.runWithDefault(for {
            e <- ResolveEngine(Car(2L, "bar"))
            e <- Monad.Unit(e)
          } yield(e))
          engine12 shouldBe Engine(0L, 0L)
        }
      }
    }

    it("should run monads created by different operations") {
      val car = Car(3L, "foo")
      val m1 = ResolveEngine(car).flatMap { _ =>
        Monad.Unit(Crankshaft(1001L))
      }
      val m2 = Monad.Unit(Engine(0L, 0L)).flatMap { _ =>
        Monad.Unit(Crankshaft(1003L))
      }
      val seq1: Seq[Monad[Crankshaft]] = Seq(m1, m2)
      val seq2 = Seq(m1, m2)

      Monad.Divergent.run(seq1) should contain only (
        Crankshaft(1001L),
        Crankshaft(1003L)
      )
      Monad.Divergent.run(seq2) should contain only (
        Crankshaft(1001L),
        Crankshaft(1003L)
      )

      val m3 = Monad.Unit(Car(3L, "foo")).flatMap { c =>
        Monad.Unit(Engine(0L, 0L))
      }
      val m4 = Monad.Unit(Car(3L, "foo")).flatMap(ResolveEngine(_))
      val seq3 = Seq(m3, m4)
      Monad.Divergent.run(seq3) should contain only (
        Engine(0L, 0L),
        Engine(1003L, 3L)
      )
    }

    it("should resolve for each element of a list") {
      locally {
        val c = new Counter
        val engine1: Option[Engine] =
          Monad.Divergent.run(CountedResolveEngine(Car(3L, "foo"), c))
        engine1 shouldBe Some(Engine(1003L, 3L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine2: Option[Engine] =
          Monad.Divergent.run(CountedResolveEngine(Car(2L, "bar"), c))
        engine2 shouldBe None
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms1 = Seq(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        )
        val engines1: Seq[Engine] = Monad.Divergent.run(ms1)
        engines1 should contain only (Engine(1003L, 3L), Engine(1005L, 5L))
        c.count shouldBe 3
      }

      locally {
        val c = new Counter
        val ms2 = Array(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        ).toSeq
        val engines2: Seq[Engine] = Monad.Divergent.run(ms2)
        engines2 shouldBe Array(Engine(1003L, 3L), Engine(1005L, 5L))
        c.count shouldBe 3
      }

      locally {
        val c = new Counter
        val engine3: Option[Engine] =
          Monad.Divergent.run(CountedResolveEngine(Car(3L, "foo"), c).map { e =>
            Engine(1002L, 5L)
          })
        engine3 shouldBe Some(Engine(1002L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine4: Option[Engine] = Monad.Divergent.run(for {
          e <- CountedResolveEngine(Car(3L, "foo"), c)
        } yield(Engine(1002L, 5L)))
        engine4 shouldBe Some(Engine(1002L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine5: Option[Engine] =
          Monad.Divergent.run(CountedResolveEngine(Car(2L, "bar"), c).map { e =>
            Engine(1002L, 5L)
          })
        engine5 shouldBe None
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine6: Option[Engine] = Monad.Divergent.run(for {
          e <- CountedResolveEngine(Car(2L, "bar"), c)
        } yield(Engine(1002L, 5L)))
        engine6 shouldBe None
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms3 = Seq(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        ).map { m => m.map { e => Engine(e.id + 1, e.carId + 1) } }
        val engines3: Seq[Engine] = Monad.Divergent.run(ms3)
        engines3 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
        c.count shouldBe 3
      }

      locally {
        val c = new Counter
        val ms4 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- CountedResolveEngine(car, c)
        } yield(Engine(e.id + 1, e.carId + 1)) }
        val engines4: Seq[Engine] = Monad.Divergent.run(ms4)
        engines4 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
        c.count shouldBe 3
      }

      locally {
        val c = new Counter
        val engine3: Option[Engine] =
          Monad.Divergent.run(CountedResolveEngine(Car(3L, "foo"), c).flatMap { e =>
            Monad.Unit(Engine(1002L, 5L))
          })
        engine3 shouldBe Some(Engine(1002L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine4: Option[Engine] = Monad.Divergent.run(for {
          e <- CountedResolveEngine(Car(3L, "foo"), c)
          e <- Monad.Unit(Engine(1002L, 5L))
        } yield(e))
        engine4 shouldBe Some(Engine(1002L, 5L))
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val engine5: Option[Engine] =
          Monad.Divergent.run(CountedResolveEngine(Car(3L, "foo"), c).flatMap { e =>
            CountedResolveEngine(Car(5L, "baz"), c)
          })
        engine5 shouldBe Some(Engine(1005L, 5L))
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val engine6: Option[Engine] = Monad.Divergent.run(for {
          e <- CountedResolveEngine(Car(3L, "foo"), c)
          e <- CountedResolveEngine(Car(5L, "baz"), c)
        } yield(e))
        engine6 shouldBe Some(Engine(1005L, 5L))
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val engine7: Option[Engine] =
          Monad.Divergent.run(CountedResolveEngine(Car(3L, "foo"), c).flatMap { e =>
            CountedResolveEngine(Car(2L, "bar"), c)
          })
        engine7 shouldBe None
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val engine8: Option[Engine] = Monad.Divergent.run(for {
          e <- CountedResolveEngine(Car(3L, "foo"), c)
          e <- CountedResolveEngine(Car(2L, "bar"), c)
        } yield(e))
        engine8 shouldBe None
        c.count shouldBe 2
      }

      locally {
        val c = new Counter
        val ms3 = Seq(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        ).map(_.flatMap { e => Monad.Unit(Engine(e.id + 1, e.carId + 1)) })
        val engines3: Seq[Engine] = Monad.Divergent.run(ms3)
        engines3 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
        c.count shouldBe 3
      }

      locally {
        val c = new Counter
        val ms4 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- CountedResolveEngine(car, c)
          e <- Monad.Unit(Engine(e.id + 1, e.carId + 1))
        } yield(e) }
        val engines4: Seq[Engine] = Monad.Divergent.run(ms4)
        engines4 shouldBe Array(Engine(1004L, 4L), Engine(1006L, 6L))
        c.count shouldBe 3
      }

      locally {
        val c = new Counter
        val ms5 = Seq(
          CountedResolveEngine(Car(3L, "foo"), c),
          CountedResolveEngine(Car(2L, "bar"), c),
          CountedResolveEngine(Car(5L, "baz"), c)
        ).map(_.flatMap { e =>
          CountedResolveEngine(Car(e.carId+2, "dummy"), c)
        })
        val engines5: Seq[Engine] = Monad.Divergent.run(ms5)
        engines5 shouldBe Array(Engine(1005L, 5L), Engine(1007L, 7L))
        c.count shouldBe 5
      }

      locally {
        val c = new Counter
        val ms6 = Seq(
          Car(3L, "foo"),
          Car(2L, "bar"),
          Car(5L, "baz")
        ).map { car => for {
          e <- CountedResolveEngine(car, c)
          e <- CountedResolveEngine(Car(e.carId+2, "dummy"), c)
        } yield(e) }
        val engines6: Seq[Engine] = Monad.Divergent.run(ms6)
        engines6 shouldBe Array(Engine(1005L, 5L), Engine(1007L, 7L))
        c.count shouldBe 5
      }

      val ms1: Seq[Monad.Unit[Engine]] = Seq.empty

      locally {
        val c = new Counter
        val ms6 = ms1.map { m => m.flatMap {
          e => CountedResolveEngine(Car(3L, "foo"), c)
        } }
        val engines6: Seq[Engine] = Monad.Divergent.run(ms6)
        engines6 shouldBe empty
        c.count shouldBe 0
      }

      locally {
        val c = new Counter
        val ms7 = ms1.map { m => for {
          e <- m
          e <- CountedResolveEngine(Car(3L, "foo"), c)
        } yield(e) }
        val engines7: Seq[Engine] = Monad.Divergent.run(ms7)
        engines7 shouldBe empty
        c.count shouldBe 0
      }

      locally {
        val c = new Counter
        val ms8 = Seq(Car(2L, "bar")).map(CountedResolveEngine(_, c))
        val engines8: Seq[Engine] = Monad.Divergent.run(ms8)
        engines8 shouldBe empty
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms8 = Seq(Car(2L, "bar")).map(CountedResolveEngine(_, c))
        val ms13 = ms8.map { m => m.flatMap {
          e => CountedResolveEngine(Car(3L, "foo"), c)
        } }
        val engines13: Seq[Engine] = Monad.Divergent.run(ms13)
        engines13 shouldBe empty
        c.count shouldBe 1
      }

      locally {
        val c = new Counter
        val ms8 = Seq(Car(2L, "bar")).map(CountedResolveEngine(_, c))
        val ms14 = ms8.map { m => for {
          e <- m
          e <- CountedResolveEngine(Car(3L, "foo"), c)
        } yield(e) }
        val engines14: Seq[Engine] = Monad.Divergent.run(ms14)
        engines14 shouldBe empty
        c.count shouldBe 1
      }
    }
  }
}

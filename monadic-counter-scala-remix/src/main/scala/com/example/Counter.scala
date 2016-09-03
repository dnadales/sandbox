package com.example

import cats.data._
import cats.data.WriterT
import cats.instances.all._
import cats._
import cats.arrow.Arrow

/**
  * Created by damian on 31-8-16.
  */
object Counter {

  case class Counter(cValue: Int) extends AnyVal {
    def inc(n: Int): Counter = Counter(cValue + n)
  }

  // The computation we want to run.
  val mComputation: Counter =
  Counter(0)
    .inc(3)
    .inc(3)
    .inc(3)
    .inc(5)
    .inc(5)

  // Introducing state.
  type CounterS[A] = State[Counter, A]

  def incS(n: Int): CounterS[Unit] = {//State[Counter, Unit] = {
    State.modify(c => c.inc(n))
  }

  val mComputationS: CounterS[Unit] = for {
    _ <- incS(3)
    _ <- incS(3)
    _ <- incS(3)
    _ <- incS(5)
    _ <- incS(5)
  } yield ()

  // Introducing an environment.
  type CounterRS[A] = ReaderT[CounterS[?], Int, A]

  def incR: CounterRS[Unit] = {
    Kleisli
      .ask[CounterS[?], Int]
      .flatMap{n:Int => ReaderT.lift(incS(n))}
  }

  def local(n: Int) = Kleisli.local[CounterS[?], Unit, Int](_ => n)(_)

  val mComputationRS: CounterRS[Unit] =
    local(3) {
      for {
        _ <- incR
        _ <- incR
        _ <- incR
        _ <- local(5) {
          for {
            _ <- incR
            _ <- incR
          } yield ()
        }
      } yield ()
    }

  // Adding logging.
  type CounterWRS[A] = WriterT[CounterRS[?], Seq[Int], A]

  // WriterT does not have lift in the latest release of Cats.
  def incW: CounterWRS[Unit] = ???

}

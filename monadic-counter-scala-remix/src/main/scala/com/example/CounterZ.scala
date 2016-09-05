package com.example

import scalaz._
import Scalaz._



/**
  * Created by damian on 3-9-16.
  */
object CounterZ {
  val SM = ReaderWriterState.rwstMonad[Id, Int, List[Int], Counter.Counter]

  type CounterZ[A] = ReaderWriterState[Int, List[Int], Counter.Counter, Unit]

  val incZ: IRWS[Int, List[Int], Counter.Counter, Counter.Counter, Unit] = for {
    n <- SM.ask
    _ <- SM.modify(c => c.inc(n))
    c <- SM.get
    _ <- SM.tell(List(c.cValue))
  } yield ()


  val mComputationZ:
  IRWS[Int, List[Int], Counter.Counter, Counter.Counter, Unit] = for {
    _ <- SM.local[Unit](i => 3) {
      for {
        _ <- incZ
        _ <- incZ
        _ <- incZ
        _ <- SM.local[Unit](i => 5) {
          for {
            _ <- incZ
            _ <- incZ
          } yield ()
        }
      } yield ()
    }
  } yield ()
}

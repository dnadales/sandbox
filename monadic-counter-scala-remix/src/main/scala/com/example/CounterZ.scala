package com.example

import com.example.Counter.Counter

import scalaz.Scalaz._
import scalaz._
import scalaz.syntax.applicative._
import scalaz.syntax.kleisli._

/**
  * Created by damian on 3-9-16.
  */
object CounterZ {

  type CounterZ[A] = ReaderWriterState[Int, Seq[Int], Counter.Counter, Unit]

  val incZ: CounterZ[Unit] =
    ???

  val mComputationZ: CounterZ[Unit] =
    //ReaderWriterStateT.rwstMonad.local(_ => 3)(incZ)
    // This won't work either. So much for Scala for today...
    //Kleisli.local[Id, Unit, Int](_ => 5)(incZ)



}

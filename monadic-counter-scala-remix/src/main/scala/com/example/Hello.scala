package com.example

import Counter._

object Hello {
  def main(args: Array[String]): Unit = {
    println(s"Counter: ${mComputation}")

    println(s"State counter: ${mComputationS.run(new Counter(0)).value._1}")

    val resultRSCounter = mComputationRS.run(0).run(new Counter(0)).value._1

    println(s"Reader counter: $resultRSCounter")
  }
}

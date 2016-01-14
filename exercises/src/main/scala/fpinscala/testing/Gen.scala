package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(s => f(s).sample))
}

object Gen {

  def ints: Gen[Int] = Gen(State(RNG.int))

  def choose(start: Int, stopExcl: Int): Gen[Int] = Gen(State { rng =>
    val (i, rng2) = RNG.int(rng)
    val range = stopExcl - start
    (start + (i % range), rng2)
  })

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = choose(0,99999).map(_ % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
    Gen(State.sequence(List.fill(n)(g.sample)))

  def pair: Gen[(Int, Int)] = Gen {
    ints.sample.map2(ints.sample) { (a,b) => (a, b) }
  }

  def range(n: Int): Gen[(Int, Int)] = ints.flatMap { i =>
    choose(i, i + n).map(x => (i, x))
  }
}

// trait Gen[A] {
//   def map[A,B](f: A => B): Gen[B] = ???
//   def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
// }

// trait SGen[+A] {

// }


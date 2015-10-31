package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) (0, rng2)
    else (math.abs(i), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    ((i.toDouble / Int.MaxValue.toDouble), rng2)
  }

  def doubleViaMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt) { i => (i.toDouble / Int.MaxValue.toDouble) } (rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(i: Int, r: RNG = rng, ints: List[Int] = Nil): (List[Int], RNG) =
      if (i == count) (ints, r)
      else {
        val (n, r2) = r.nextInt
        loop(i + 1, r2, n :: ints)
      }
    loop(0)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rngA) = ra(rng)
    val (b, rngB) = rb(rngA)
    (f(a, b), rngB)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)(_ -> _)

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List[A]())) { (a, b) => map2(b, a)(_ :: _) }

  // rng => {
  //   fs.foldLeft((List[A](), rng)) { case ((list, r), f) =>
  //     val (n, r2) = f(r)
  //     (n :: list, r2)
  //   }
  // }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rngA) = f(rng)
    g(a)(rngA)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.reverse.foldLeft(unit[S, List[A]](Nil)) { (a, b) => b.map2(a)(_ :: _) }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object CandyMachine {

  def applyInput(i: Input)(m: Machine): Machine = (i, m) match {
    case (_, Machine(_, 0, _)) => m
    case (Coin, Machine(true, _, c)) => m.copy(locked = false, coins = c + 1)
    case (Turn, Machine(false, c, _)) => m.copy(locked = true, candies = c - 1)
    case _ => m
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m => {
    val f = (inputs map (applyInput _)) reduce (_ andThen _)
    val me = f(m)
    ((me.coins, me.candies), me)
  })

  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val x = State.sequence(inputs map ((applyInput _) andThen State.modify[Machine]))
    for {
      _ <- x
      m <- State.get
    } yield (m.coins, m.candies)
  }
}

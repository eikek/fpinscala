package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take2(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Cons(h,t) => Cons(h, () => t().take(n - 1))
      case _ => this
    }

  def take(n: Int): Stream[A] =
    unfold(0) { i =>
      if (i >= n) None
      else drop(i) match {
        case Cons(h,t) => Some(h() -> (i + 1))
        case _ => None
      }
    }

  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Cons(h,t) => t().drop(n -1)
      case _ => this
    }

  def takeWhile2(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) { 
      case s@Cons(h,t) if p(h()) => Some(h() -> s.drop(1))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A]) { (a, stream) =>
      if (p(a)) Cons(() => a, () => stream.takeWhile2(p))
      else empty
    }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
    case _ => true
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    unfold(this) { str =>
      str.headOption match {
        case Some(a) => Some(f(a) -> str.drop(1))
        case None => None
      }
    }

  def map2[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B]) { (a, str) => Cons(() => f(a), () => str) }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A]){ (a, str) =>
      if (p(a)) cons(a, str.filter(p)) else str.filter(p)
    }

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other) { (a, stream) => cons(a, stream) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B]){ (a, stream) => f(a).append(stream) }

  def startsWith[B](s: Stream[B]): Boolean = this.zipAll(s)
    .takeWhile({case (opta, optb) => optb.nonEmpty})
    .forAll({ case (opta, optb) => opta == optb })

  def toList: List[A] = foldRight(Nil: List[A])((a, list) => a :: list)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold(this -> s2) { case (a, b) =>
      (a.headOption, b.headOption) match {
        case (None, None) => None
        case (x, y) => Some((x->y, a.drop(1) -> b.drop(1)))
      }
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(a: Int, b: Int): Stream[Int] = cons(a, fibs(b, a+b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
}

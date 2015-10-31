package fpinscala.laziness

import org.scalatest.FlatSpec
import Stream._

class StreamTest extends FlatSpec {

  val l = List(1,2,3)

  "toList(Stream(1,2,3)" should "be [1,2,3]" in {
    assert(Stream(1,2,3).toList == l)
  }

  "Stream(1,2,3).take(2)" should "be Stream(1,2)" in {
    assert(Stream(1,2,3).take(2).toList == Stream(1,2).toList)
  }

  "drop([1,2,3], 1)" should "be Stream.tail([1,2,3])" in {
    assert(Stream(1,2,3).drop(1).toList == Stream(2,3).toList)
  }

  "ones.take(5)" should "be [1,1,1,1,1]" in {
    assert(Stream.ones.take(5).toList == List(1,1,1,1,1))
  }

  "Stream(1,2,3,4).takeWhile(_ < 3)" should "be [1,2]" in {
    assert(Stream(1,2,3,4).takeWhile(_ < 3).toList == List(1,2))
    assert(Stream(1,2,3,4).takeWhile2(_ < 3).toList == List(1,2))
  }

  "ones.forAll(_ > 3)" should "be false" in {
    assert(Stream.ones.forAll(_ > 3) == false)
  }

  "Stream(1,2,3).forAll(_ > 0)" should "be true" in {
    assert(Stream(1,2,3).forAll(_ > 0))
  }

  "Stream(1,2,3).map(_ + 2)" should "be Stream(3,4,5)" in {
    assert(Stream(1,2,3).map(_ + 2).toList == List(3,4,5))
  }

  "ones.map(_ + 1)" should "be twos" in {
    assert(ones.map(_ + 2).take(3).toList == List(3,3,3))
  }

  "Stream(1,2,3).filter(_ > 2)" should "be Stream(3)" in {
    assert(Stream(1,2,3).filter(_ > 2).toList == List(3))
  }

  "Stream(1,2,3).append(Strean(4,5)" should "be Stream(1,2,3,4,5)" in {
    assert(Stream(1,2,3).append(Stream(4,5)).toList == List(1,2,3,4,5))
  }

  "Stream(1,2,3).flatMap(i => Stream(i,i))" should "be Stream(1,1,2,2,3,3)" in {
    assert(Stream(1,2,3).flatMap(i => Stream(i,i)).toList == List(1,1,2,2,3,3))
  }

  "constant(2).take(5)" should "be Strean(2,2,2,2,2)" in {
    assert(constant(2).take(5).toList == List(2,2,2,2,2))
  }

  "from(5).take(4)" should "be Stream(5,6,7,8)" in {
    assert(from(5).take(4).toList == List(5,6,7,8))
  }

  "fibs(0,1).take(7)" should "be [0,1,1,2,3,5,8]" in {
    assert(fibs(0,1).take(7).toList == List(0,1,1,2,3,5,8))
  }

  "unfold(0)(n => Some(n, n+1))" should "be from(0)" in {
    assert(unfold(0)(n => Some((n, n+1))).take(5).toList == from(0).take(5).toList)
  }

  "ones.headOption" should "be Some(1)" in {
    assert(ones.headOption == Some(1))
  }

  "Stream(2,3).headOption" should "be Some(2)" in {
    assert(Stream(2,3).headOption == Some(2))
  }

  "Stream.empty.headOption" should "be None" in {
    assert(empty.headOption == None)
  }

  "Stream(1,2,3).zipAll(Stream(5,4,3))" should "be [(1,5),(2,4),(3,3)]" in {
    assert {
      Stream(1,2,3).zipAll(Stream(5,4,3)).toList == List.apply(
        Some(1) -> Some(5),
        Some(2) -> Some(4),
        Some(3) -> Some(3)
      )
    }
  }

  "Stream(1,2,3).startsWith(Stream(1,2))" should "be true" in {
    assert(Stream(1,2,3).startsWith(Stream(1,2)) == true)
  }

  "Stream(1,2,3).tails" should "be Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())" in {

    assert(Stream(1,2,3).tails.map(_.toList).toList == List(List(1,2,3), List(2,3), List(3), List()))
  }
}

package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  val l = List(1,2,3)

  "tail of [1,2,3]" should "be [2,3]" in {
    assert(List.tail(l) == List(2,3))
  }

  "tail of [1]" should "be nil" in {
    assert(List.tail(List(1)) == Nil)
  }

  "setHead([1,2,3], 5)" should "be [5,2,3]" in {
    assert(List.setHead(l, 5) == List(5,2,3))
    assert(List.tail(List.setHead(l, 5)) eq List.tail(l))
  }

  "drop([1,2,3], 1)" should "be List.tail([1,2,3])" in {
    assert(List.drop(l, 1) eq List.tail(l))
  }

  "drop([1,2,3], 2)" should "be List(3)" in {
    assert(List.drop(l, 2) == List(3))
  }

  "drop([1,2,3], 3)" should "be Nil" in {
    assert(List.drop(l, 3) == Nil)
  }

  "dropWhile([1,2,3], _ > 0)" should "be Nil" in {
    assert(List.dropWhile[Int](l, _ > 0) == Nil)
  }

  "dropWhile([1,2,3], _ < 2)" should "be [2,3]" in {
    assert(List.dropWhile[Int](l, _ < 2) == List.tail(l))
  }

  "dropWhile([1,2,3], _ < 0)" should "be the list itself" in {
    assert(List.dropWhile[Int](l, _ < 0) eq l)
  }

  "init([1,2,3])" should "be [1,2]" in {
    assert(List.init(l) == List(1,2))
  }

  "length([1,2,3])" should "be 3" in {
    assert(List.length(l) == 3)
  }

  "length(Nil)" should "be 0" in {
    assert(List.length(Nil) == 0)
  }

  "foldLeft([1,2,3], 0)( (_, i) => i + 1)" should "be 3" in {
    assert(List.foldLeft(l, 0) { (i, _) => i + 1 } == 3)
  }

  "foldLeft([1,2,3],0)(_ + _)" should "be 6" in {
    assert(List.foldLeft(l, 10)(_ + _) == 16)
  }

  "foldRightViaFoldLeft([1,2,3], 0)(_ - _)" should "be foldRight([1,2,3], 0)(_ - _)" in {
    assert(List.foldRightViaFoldLeft(l, Nil:List[Int])(Cons(_, _)) == List.foldRight(l, Nil:List[Int])(Cons(_, _)))
  }

  "reverse of [1,2,3]" should "be [3,2,1]" in {
    assert(List.reverse(l) == List(3,2,1))
  }

  "map +1 over [1,2,3]" should "be [2,3,4]" in {
    assert(List.map(l)(_ + 1) == List(2,3,4))
  }

  "append2" should "work like append" in {
    assert(List.append2(l, l) == List.append(l, l))
  }

  "concat([[1,2],[3,4],[5,6]])" should "be [1,2,3,4,5,6]" in {
    assert(List.concat(List(List(1,2), List(3,4), List(5,6))) == List(1,2,3,4,5,6))
  }

  "filter([1,2,3])(_ % 2 == 0)" should "be [2]" in {
    assert(List.filter(l)(_ % 2 == 0) == List(2))
    assert(List.filterViaFlatMap(l)(_ % 2 == 0) == List(2))
  }

  "flatMap([1,2,3])(i => List(i,i))" should "be [1,1,2,2,3,3]" in {
    assert(List.flatMap(l)(i => List(i,i)) == List(1,1,2,2,3,3))
  }

  "addInts([1,2,3],[1,2,3])" should "be [2,4,6]" in {
    assert(List.addInts(l, l) == List(2,4,6))
  }

  "zipWitn([1,2,3],[1,2,3])(_ + _)" should "be [2,4,6]" in {
    assert(List.zipWith(l, l)(_ + _) == List(2,4,6))
  }

  "take([1,2,3], 2)" should "be [1,2]" in {
    assert(List.take(l, 2) == List(1,2))
  }

  "take([1,2], 2)" should "be [1,2]" in {
    assert(List.take(List(1,2), 2) == List(1,2))
  }

  "[2,3] is subsequence of [1,2,3]" should "be true" in {
    assert(List.hasSubsequence(l, List(2,3)))
  }
}

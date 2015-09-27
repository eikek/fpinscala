package fpinscala.gettingstarted

import org.scalatest.FlatSpec

class GettingStartedTest extends FlatSpec {

  "first fibonacci number" should "be 0" in {
    assert(MyModule.fib(0) == 0)
  }

  "second fibonacci number" should "be 1" in {
    assert(MyModule.fib(1) == 1)
  }

  "third fibonacci number" should "be 1" in {
    assert(MyModule.fib(2) == 1)
  }

  "sixth fibonacci number" should "be 5" in {
    assert(MyModule.fib(5) == 5)
  }

  "47th fibonacci number" should "be 1836311903" in {
    assert(MyModule.fib(46) == 1836311903)
  }

  "isSorted for [1,2,3,4]" should "be true" in {
    assert(PolymorphicFunctions.isSorted[Int](Array(1,2,3,4), _ > _))
  }

  "isSorted for [1,4,2]" should "be false" in {
    assert(!PolymorphicFunctions.isSorted[Int](Array(1,4,2), _ > _))
  }

  "isSorted for [5,4,3,2]" should "be false" in {
    assert(!PolymorphicFunctions.isSorted[Int](Array(5,4,3,2), _ > _))
  }

  "isSorted for []" should "be true" in {
    assert(PolymorphicFunctions.isSorted[Int](Array.empty, _ > _))
  }

  "isSorted for [3]" should "be true" in {
    assert(PolymorphicFunctions.isSorted[Int](Array(3), _ > _))
  }
}

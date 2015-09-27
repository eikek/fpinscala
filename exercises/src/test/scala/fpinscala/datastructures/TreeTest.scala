package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeTest extends FlatSpec {

  val t11 = Branch(Leaf(1), Leaf(1))

  val t1234 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  val t13254 = Branch(Branch(Leaf(1), Leaf(3)), Branch(Branch(Leaf(2), Leaf(5)), Leaf(4)))

  "size of t11" should "be 3" in {
    assert(Tree.size(t11) == 3)
  }

  "size of t1234" should "be 7" in {
    assert(Tree.size(t1234) == 7)
  }

  "size of t12345" should "be 9" in {
    assert(Tree.size(t13254) == 9)
  }

  "maximumInt in t11" should "be 1" in {
    assert(Tree.maximumInt(t11) == 1)
  }

  "maximumInt in t1234" should "be 4" in {
    assert(Tree.maximumInt(t1234) == 4)
  }

  "maximumInt in t13254" should "be 5" in {
    assert(Tree.maximumInt(t13254) == 5)
  }

  "depth of t11" should "be 1" in {
    assert(Tree.depth(t11) == 1)
  }

  "depth of t1234" should "be 2" in {
    assert(Tree.depth(t1234) == 2)
  }

  "depth of t13254" should "be 3" in {
    assert(Tree.depth(t13254) == 3)
  }

  "map +1 over [11]" should "be [22]" in {
    assert(Tree.map(t11)(_ + 1) == Branch(Leaf(2), Leaf(2)))
  }

  "map -1 over [13254]" should "be [02143]" in {
    assert(Tree.map(t13254)(_ - 1) == Branch(Branch(Leaf(0), Leaf(2)), Branch(Branch(Leaf(1), Leaf(4)), Leaf(3))))
  }
}

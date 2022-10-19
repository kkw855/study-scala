package book.red_fp.ch3

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TreeTest extends AnyFunSuite, Matchers:
  import Tree._

  private val tree =
    Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  test("size") {
    sizeViaFold(tree) shouldBe 7
  }

  test("maximum") {
    maximumViaFold(tree) shouldBe 4
  }

  test("depth") {
    depthViaFold(tree) shouldBe 2
  }

  test("map") {
    mapViaFold(tree)(_ * 10) shouldBe Branch(
      Branch(Leaf(10), Leaf(20)),
      Branch(Leaf(30), Leaf(40))
    )
  }

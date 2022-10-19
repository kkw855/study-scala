package book.red_fp.ch3

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

case class Box[+A](value: A)

trait Pet:
  val name: String

class Cat(val name: String) extends Pet
class Dog(val name: String) extends Pet

class ListTest extends AnyFunSuite, Matchers:
  import List._

  test("sum") {
    sum(List(1, 2, 3)) shouldBe 6
  }

  test("product") {
    product(List(1.0, 2.0, 5.0)) shouldBe 10.0
    product(List(1.0, 0, 5.0)) shouldBe 0.0
  }

  test("tail") {
    an[RuntimeException] should be thrownBy tail(Nil)
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  test("setHead") {
    an[RuntimeException] should be thrownBy setHead(10, Nil)
    setHead(10, List(1, 2, 3)) shouldBe List(10, 2, 3)
  }

  test("drop") {
    drop(Nil, 10) shouldBe Nil
    drop(List(1, 2, 3), 4) shouldBe Nil
    drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
    drop(List(1, 2, 3), 1) shouldBe List(2, 3)
    drop(List(1, 2, 3, 4), 2) shouldBe List(3, 4)
  }

  test("dropWhile") {
    val list = List(1, 2, 3, 4)
    val p: Int => Boolean = x => x <= 2

    dropWhile(Nil, p) shouldBe Nil
    dropWhile(list, p) shouldBe List(3, 4)
  }

  test("append") {
    append(Nil, Nil) shouldBe Nil
    append(List(1, 2, 3, 4), Nil) shouldBe List(1, 2, 3, 4)
    append(Nil, List(5, 6, 7)) shouldBe List(5, 6, 7)
    append(List(1, 2, 3, 4), List(5, 6, 7)) shouldBe List(1, 2, 3, 4, 5, 6, 7)
  }

  test("init") {
    an[RuntimeException] should be thrownBy init(Nil)
    init(List(1, 2, 3)) shouldBe List(1, 2)
  }

  test("length") {
    List.length(List(1, 2, 3)) shouldBe 3
  }

  test("reverse") {
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  test("appendViaFoldRight") {
    appendViaFoldRight(Nil, Nil) shouldBe Nil
    appendViaFoldRight(List(1, 2, 3, 4), Nil) shouldBe List(1, 2, 3, 4)
    appendViaFoldRight(Nil, List(5, 6, 7)) shouldBe List(5, 6, 7)
    appendViaFoldRight(List(1, 2, 3, 4), List(5, 6, 7)) shouldBe List(1, 2, 3,
      4, 5, 6, 7)
  }

  test("plusOne") {
    plusOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

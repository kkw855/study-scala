package book.red_fp.ch2

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MyModuleTest extends AnyFunSuite, Matchers:
  import MyModule._

  test("formatAbs") {
    formatAbs(-1) shouldBe "The absolute value of -1 is 1."
    formatAbs(0) shouldBe "The absolute value of 0 is 0."
    formatAbs(1) shouldBe "The absolute value of 1 is 1."
  }

  test("factorial") {
    factorial(-1) shouldBe 1
    factorial(0) shouldBe 1
    factorial(1) shouldBe 1
    factorial(2) shouldBe 2
    factorial(3) shouldBe 6
    factorial(4) shouldBe 24
  }

  test("formatFactorial") {
    formatFactorial(4) shouldBe "The factorial of 4 is 24."
  }

  // Exercise 2.1
  test("fibonacci") {
    fibonacci(-1) shouldBe 0
    fibonacci(0) shouldBe 0
    fibonacci(1) shouldBe 1
    fibonacci(2) shouldBe 1
    fibonacci(3) shouldBe 2
    fibonacci(4) shouldBe 3
    fibonacci(5) shouldBe 5
  }

  test("formatResult") {
    formatResult(
      "absolute value",
      -42,
      abs
    ) shouldBe "The absolute value of -42 is 42."
    formatResult("factorial", 4, factorial) shouldBe "The factorial of 4 is 24."
  }

  test("findFirstMono") {
    findFirstMono(Array("hi", "hello", "world"), "unknown") shouldBe -1
    findFirstMono(Array("hi", "hello", "world"), "hello") shouldBe 1
  }

  test("findFirstPoly") {
    findFirstPoly(
      Array("hi", "hello", "world"),
      (x: String) => x.contains("unknown")
    ) shouldBe -1
    findFirstPoly(
      Array("hi", "hello", "world"),
      x => x.contains("llo")
    ) shouldBe 1

    findFirstPoly(Array(1, 2, 3), (x: Int) => x > 100) shouldBe -1
    findFirstPoly(Array(1, 2, 3), x => x > 1) shouldBe 1
  }

  // Exercise 2.2
  test("isSorted") {
    isSorted(Array(1, 2, 4, 3), (x, y) => x <= y) shouldBe false
    isSorted(Array(1, 2, 4, 3), (x, y) => x <= y) shouldBe false
  }

  test("partial1") {
    val ten = partial1(10, (a: Int, b: Int) => a * b)

    ten(4) shouldBe 40
  }

  test("curry") {
    val curriedA: String => String => String = curry((a: String, b: String) => a ++ b)
    
    val curriedB: String => String = curriedA("Hello ")
    
    curriedB("World!") shouldBe "Hello World!"
  }
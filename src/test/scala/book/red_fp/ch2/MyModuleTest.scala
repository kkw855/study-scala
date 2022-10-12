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

  test("fibonacci") {
    fibonacci(-1) shouldBe 0
    fibonacci(0) shouldBe 0
    fibonacci(1) shouldBe 1
    fibonacci(2) shouldBe 1
    fibonacci(3) shouldBe 2
    fibonacci(4) shouldBe 3
    fibonacci(5) shouldBe 5
  }

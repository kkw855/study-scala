package book.red_fp.ch4

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EitherTest extends AnyFunSuite, Matchers:
  import Either._

  test("parseInsuranceRateQuote") {
    parseInsuranceRateQuote("2", "hello") shouldBe a[Left[_]]
    parseInsuranceRateQuote("2", "3") shouldBe Right(6.5)
  }

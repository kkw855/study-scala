package book.red_fp.ch4

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class OptionTest extends AnyFunSuite, Matchers:
  import Option._

  test("parseInsuranceRateQuote") {
    parseInsuranceRateQuote("2", "hello") shouldBe None
    parseInsuranceRateQuote("2", "3") shouldBe Some(6.5)
  }

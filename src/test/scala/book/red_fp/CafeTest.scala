package book.red_fp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CafeTest extends AnyFunSuite, Matchers {
  private val cafe = Cafe()
  private val creditA = CreditCard("A")
  private val creditB = CreditCard("B")
  private val charges = List(
    Charge(creditA, 1000),
    Charge(creditB, 100),
    Charge(creditA, 200),
    Charge(creditB, 1000)
  )

  test("testBuyCoffee") {
    cafe.buyCoffee(creditA) shouldBe (Coffee(1200), Charge(creditA, 1200))
    cafe.buyCoffee(creditB) shouldBe (Coffee(1200), Charge(creditB, 1200))
  }

  test("testCoalesce") {
    cafe.buyCoffees(creditA, 3) shouldBe
      (List(Coffee(1200), Coffee(1200), Coffee(1200)), Charge(creditA, 3600))
  }

  test("testBuyCoffees") {
    cafe.coalesce(charges) should contain theSameElementsAs List(
      Charge(creditA, 1200),
      Charge(creditB, 1100)
    )
  }
}

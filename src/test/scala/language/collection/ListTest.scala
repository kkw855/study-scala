package language.collection

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFunSuite, Matchers {
  private case class Coffee(name: String, amount: Double)
  
  private val list = List(1, 2, 3)

  test("[추가] 리스트 맨 뒤에 엘리먼트를 추가한다.") {
    list :+ 4 shouldBe List(1, 2, 3, 4)
  }

  test("zip, unzip") {
    val ids = List(101, 102, 103)
    val names = List("Name1", "Name2", "Name3")
    val zipped = ids zip names

    zipped shouldBe List((101, "Name1"), (102, "Name2"), (103, "Name3"))

    val (ids2, names2) = zipped.unzip

    ids2 shouldBe ids
    names2 shouldBe names
  }

  // noinspection SimplifiableFoldOrReduce
  test("reduce") {
    list.reduce(_ + _) shouldBe 10
  }

  test("groupBy") {
    val coffees = List(
      Coffee("Americano", 1200.5),
      Coffee("Espresso", 1600.2),
      Coffee("Americano", 800.13),
      Coffee("Espresso", 1300.7)
    )

    coffees.groupBy(_.name) shouldBe Map(
      "Americano" -> List(
        Coffee("Americano", 1200.5),
        Coffee("Americano", 800.13)
      ),
      "Espresso" -> List(Coffee("Espresso", 1600.2), Coffee("Espresso", 1300.7))
    )
  }
}

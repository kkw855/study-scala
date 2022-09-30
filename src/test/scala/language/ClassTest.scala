package language

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.*

import scala.concurrent.ExecutionContext

class Point(var x: Int = 0, var y: Int = 0):
  def move(dx: Int, dy: Int): Unit =
    x += dx
    y += dy

  override def toString: String =
    s"$x, $y"
end Point

class Point2:
  private var _x = 0
  private var _y = 0
  private val bound = 100

  // getter
  def x: Int = _x
  // setter
  def x_=(newValue: Int): Unit =
    if newValue < bound then _x = newValue
    else printWarning()

  def y: Int = _y
  def y_=(newValue: Int): Unit =
    if newValue < bound then _y = newValue
    else printWarning()

  private def printWarning(): Unit =
    println("WARNING: Out of bounds")

  override def toString: String =
    s"$x, $y"
end Point2

class ClassTest extends AnyFunSuite with Matchers {
  test("생성자") {
    Point(1, 2).toString shouldBe "1, 2"
    Point(3).toString shouldBe "3, 0"
    Point(y = 4).toString shouldBe "0, 4"

    val p = Point(0, 4)
    p.x = 100

    p.toString shouldBe "100, 4"
  }

  test("private 멤버") {
    val p = Point2()
    ExecutionContext
    p.x = 1
    p.y = 2

    p.x shouldBe 1
    p.y shouldBe 2
  }

  test("") {
    class Point3(x: Int, y: Int)
    val point = Point3(1, 2)
  }
}

package language

import scala.concurrent.ExecutionContext
import scala.util.Random

import concurrent.ExecutionContext.Implicits.global

object Hello extends App {
  class Point(var x: Int, var y: Int):
    def move(dx: Int, dy: Int): Unit =
      x = x + dx
      y = y + dy

    override def toString =
      s"$x, $y"

  object CustomerID:
    def apply(name: String) = s"$name--${Random.nextLong()}"

    def unapply(customerID: String): Option[String] =
      val stringArray: Array[String] = customerID.split("--")
      if stringArray.tail.nonEmpty then Some(stringArray.head) else None

  val customer1ID = CustomerID("Sukyoung") // Sukyoung--23098234908
  println(customer1ID)
  customer1ID match
    case CustomerID(name) => println(name) // prints Sukyoung
    case _                => println("Could not extract a CustomerID")

  "ABC" match
    case CustomerID(name) => println(name)
    case _                => println("Could not extract a CustomerID")

  val CustomerID(name2) = "--asdfasdfasdf"
  println(name2)

  case class Result[A](result: A):
    def foreach(f: A => Unit): Unit =
      println("foreach")
      f(result)

    def map[B](f: A => B): Result[B] =
      println("map")
      Result(f(result))

    def flatMap[B](f: A => Result[B]): Result[B] =
      println("flatMap")
      f(result)

    def withFilter(f: A => Boolean): Result[_] =
      println("withFilter")
      if f(result) then this else EmptyResult

  object EmptyResult extends Result[Null](null)

  val result = Result(10)
  val anotherResult = Result(2)
  val r =
    for
      _ <- result
      another <- anotherResult
    yield another

  println(r)

  abstract class Animal:
    def name: String

  case class Cat(name: String) extends Animal

  case class Dog(name: String) extends Animal

  def printAnimalNames(animals: List[Animal]): Unit =
    animals.foreach { animal =>
      println(animal.name)
    }

  val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
  val dogs: List[Dog] = List(Dog("Fido"), Dog("Rex"))

  // prints: Whiskers, Tom
  printAnimalNames(cats)

  // prints: Fido, Rex
  printAnimalNames(dogs)

  abstract class Serializer[-A]:
    def serialize(a: A): String

  val animalSerializer: Serializer[Animal] = new Serializer[Animal]() {
    def serialize(animal: Animal): String = s"""{ "name": "${animal.name}" }"""
  }
  val dogSerializer: Serializer[Dog] = new Serializer[Dog]() {
    def serialize(animal: Dog): String = s"""{ "name": "${animal.name}" }"""
  }

  val catSerializer: Serializer[Cat] = animalSerializer
  catSerializer.serialize(Cat("Felix"))

  // val catSerializer2: Serializer[Cat] = dogSerializer

  case class Centimeters(value: Double) extends AnyVal
  case class Meters(value: Double) extends AnyVal
  case class Kilometers(value: Double) extends AnyVal

  implicit val boolean: Boolean = true
  implicit def meters2centimeters(meters: Meters)(implicit
      secondArg: Boolean
  ): Centimeters = Centimeters(
    meters.value * 1000
  )
  implicit val kilometers2meters: Kilometers => Meters =
    kilometers => Meters(kilometers.value * 1000)
  val centimeters: Centimeters = Meters(2.5)
  // val meters: Meters = Kilometers(2.5)

  class LengthSyntax(value: Double) {
    def centimeters: Centimeters = Centimeters(value)

    def meters: Meters = Meters(value)

    def kilometers: Kilometers = Kilometers(value)
  }

  implicit def double2richSyntax(value: Double): LengthSyntax =
    new LengthSyntax(value)

  val length: Double = 2.5
  println(length.centimeters)
  println(length.meters)
  println(length.kilometers)

  val ctx = summon[ExecutionContext]

  given Int = 4
  def execute(url: String)(using timeout: Int): String = "{}"
  execute("https://www.baeldung.com")

  case class Second(value: Int)

  object TimeUtil {
    def doSomethingWithProcessingTime(sec: Second): String = {
      // impl logic
      s"${sec.value} seconds"
    }
  }
  object ImplicitConversion {
    given Conversion[Int, Second] = Second(_)
  }

  import ImplicitConversion.given
  val processingTime: Int = 100
  // auto conversion from Int to Second using given
  TimeUtil.doSomethingWithProcessingTime(processingTime)

  object Extension {
    extension (sec: Int) def toSecond = Second(sec)
  }

  import Extension._

  val sec: Second = 10.toSecond
}

package book.essential_effects

import cats.implicits._
import cats.effect.IO

import scala.concurrent.Future

object Ch2 extends App:
  val hw: IO[Unit] = IO.delay(println("hello world!"))
  val noNoes: IO[Int] = IO.delay[Int](throw new RuntimeException("oh noes!"))
  val twelve: IO[Int] = IO.pure(12)
  val noNoes2: IO[Int] = IO.raiseError(new RuntimeException("oh noes!"))

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  val ful: IO[Unit] = IO.fromFuture(IO(futurish))
  def futurish: Future[Unit] = Future("Hello World!")

  IO(12).map(_ + 1)
  (IO(12), IO("hi")).mapN((i, s) => s"$s: $i")

  for
    i <- IO(12)
    j <- IO(i + 1)
  yield j.toString

  // IO(println("first")).flatMap(_ => IO(println("second"))).unsafeRunAsync()

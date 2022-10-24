package book.essential_effects

import cats.effect.{ExitCode, IO, IOApp}
import scala.concurrent.duration._

object Ch2_2 extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    helloWorld.as(ExitCode.Success)

  val helloWorld: IO[Unit] =
    IO(println("Hello world!"))

// Exercise 2: Ticking Clock
object TickingClock extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)

  val tickingClock: IO[Unit] =
    for
      _ <- IO(println(System.currentTimeMillis()))
      _ <- IO.sleep(1.seconds)
      _ <- tickingClock
    yield ()

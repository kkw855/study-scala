package book.essential_effects

import cats.implicits._

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.*

object Ch2_2 extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    helloWorld.as(ExitCode.Success)

  val helloWorld: IO[Unit] =
    for
      _ <- IO(println("Hello world1"))
      _ <- IO(println("Hello world2"))
      _ <- IO(println(Thread.currentThread.getName))
      _ <- IO(println("Hello world3"))
    yield ()

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

object IO_Test extends App:
  import cats.effect.unsafe.implicits.global

  val io = IO[Int](throw new RuntimeException("Huh!!!"))
  // val io = IO(println("Hello IO!"))
//  io.unsafeRunAsync({
//    case Left(l)  => println("Left: " + l)
//    case Right(r) => println("Right: " + r)
//  })
  IO(println("hello world!")).unsafeRunSync()

object Future1 extends App:
  import scala.concurrent._

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  val hello = Future(println(s"[${Thread.currentThread.getName}] hello"))
  val world = Future(println(s"[${Thread.currentThread.getName}] world"))

  val hw1: Future[Unit] =
    for
      _ <- hello
      _ <- world
    yield ()

  Await.ready(hw1, 5.seconds)

  val hw2: Future[Unit] = (hello, world).mapN((_, _) => ())

  Await.ready(hw2, 5.seconds)

end Future1

object Future2 extends App:
  import scala.concurrent._

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  def hello = Future(println(s"[${Thread.currentThread.getName}] hello"))
  def world = Future(println(s"[${Thread.currentThread.getName}] world"))

  val hw1: Future[Unit] =
    for
      _ <- hello
      _ <- world
    yield ()

  Await.ready(hw1, 5.seconds)

  val hw2: Future[Unit] = (hello, world).mapN((_, _) => ())

  Await.ready(hw2, 5.seconds)

end Future2

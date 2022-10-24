package book.essential_effects

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

case class MyIO[A](unsafeRun: () => A):
  def map[B](f: A => B): MyIO[B] =
    MyIO(() => f(unsafeRun()))

  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    MyIO(() => f(unsafeRun()).unsafeRun())

object MyIO:
  def putStr(s: => String): MyIO[Unit] =
    MyIO(() => println(s))

// Exercise 1: Timing
object Timing:
  val clock: MyIO[Long] =
    MyIO(() => System.currentTimeMillis())

  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] =
    for
      start <- clock
      a <- action
      end <- clock
    yield (FiniteDuration(end - start, TimeUnit.MILLISECONDS), a)




object Ch1 extends App:
  val hello = MyIO.putStr("hello!")
  val world = MyIO.putStr("world!")

  val helloWorld: MyIO[Unit] =
    for
      _ <- hello
      _ <- world
    yield ()

  helloWorld.unsafeRun()

  val timedHello = Timing.time(MyIO.putStr("hello"))

  timedHello.unsafeRun() match {
    case (duration, _) => println(s"'hello' took $duration")
  }

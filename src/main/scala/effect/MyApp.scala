package effect

import zio.*
import zio.Console.*

import java.io.IOException

object MyApp extends ZIOAppDefault:
  override def run: ZIO[Any, Any, Any] = myAppLogic

  val myAppLogic: ZIO[Any, IOException, Unit] =
    for
      _ <- printLine("Hello! What is your name?")
      name <- readLine
      _ <- printLine(s"Hello, ${name}, welcome to ZIO!")
      _ <- printLine(User("kkw"))
    yield ()

  case class User(name: String)

object IntegrationExample {
  val runtime: Runtime[Any] = Runtime.default

  Unsafe.unsafe { implicit unsafe =>
    runtime.unsafe.run(ZIO.attempt(println("Hello World!"))).getOrThrowFiberFailure()
  }
}

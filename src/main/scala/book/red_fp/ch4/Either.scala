package book.red_fp.ch4

sealed trait Either[+E, +A]:
  def map[B](f: A => B): Either[E, B] = this match
    case l@Left(_) => l: Either[E, B]
    case Right(r) => Right(f(r))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e) => Left(e)
    case Right(a) => f(a)
  
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match
    case Left(_) => b
    case Right(a) => Right(a)
 
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (f(aa, _)))
    
  def map2_2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for 
      aa <- this
      bb <- b
    yield f(aa, bb)  
  
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either:
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    (age + numberOfSpeedingTickets) * 1.3  
    
  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    for
      a <- Try { age.toInt }
      tickets <- Try { numberOfSpeedingTickets.toInt }
    yield insuranceRateQuote(a, tickets)  
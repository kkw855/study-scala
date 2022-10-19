package book.red_fp.ch4

sealed trait Option[+A]:
  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(a) => Some(f(a))

  def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option:
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match
      case (Some(a), Some(b)) => Some(f(a, b))
      case _                  => None

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age + numberOfSpeedingTickets * 1.3

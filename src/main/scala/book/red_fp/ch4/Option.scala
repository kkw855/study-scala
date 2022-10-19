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
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for
      aa <- a
      bb <- b
    yield f(aa, bb)
  
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    (age + numberOfSpeedingTickets) * 1.3

  def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String
  ): Option[Double] =
    val optAge = Try { age.toInt }
    val optTickets = Try { numberOfSpeedingTickets.toInt }
    map2(optAge, optTickets)(insuranceRateQuote)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match
    case Nil    => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))

  def sequence_1[A, B](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map { aa => Try(aa.toInt) })

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match
    case Nil    => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

  def sequenceViaTraverse[A, B](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

package book.red_fp.ch3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match
    case Nil              => 0
    case Cons(head, tail) => head + sum(tail)

  def product(ds: List[Double]): Double = ds match
    case Nil              => 1.0
    case Cons(0.0, _)     => 0.0
    case Cons(head, tail) => head * product(tail)

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match
    case Nil         => sys.error("tail of empty list")
    case Cons(_, xs) => xs

  // Exercise 3.3
  def setHead[A](a: A, l: List[A]): List[A] = l match
    case Nil         => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(a, xs)

  // Exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else
      l match
        case Nil         => Nil
        case Cons(_, xs) => drop(xs, n - 1)

  // Exercise 3.5
  @tailrec
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match
    case Cons(x, xs) if p(x) => dropWhile(xs, p)
    case _                   => l

  def append[A](a1: List[A], a2: List[A]): List[A] = (a1, a2) match
    case (Nil, Nil)        => Nil
    case (Nil, _)          => a2
    case (_, Nil)          => a1
    case (Cons(x, Nil), _) => Cons(x, a2)
    case (Cons(x, xs), _)  => Cons(x, append(xs, a2))

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match
    case Nil          => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))

  def sum2(l: List[Int]): Int =
    foldRight(l, 0)(_ + _)

  def product2(l: List[Double]): Double =
    foldRight(l, 1.0)(_ * _)

  // Exercise 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => 1 + acc)

  // Exercise 3.10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)

  // Exercise 3.11
  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((b, a) => Cons(a, b))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  def plusOne(l: List[Int]): List[Int] = l match
    case Nil         => Nil
    case Cons(x, xs) => Cons(x + 1, plusOne(xs))

  def doubleToString(l: List[Double]): List[String] = l match
    case Nil         => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))

  def map[A, B](l: List[A])(f: A => B): List[B] = l match
    case Nil         => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))

  def filter[A](l: List[A])(p: A => Boolean): List[A] = l match
    case Nil                 => Nil
    case Cons(x, xs) if p(x) => Cons(x, filter(xs)(p))
    case Cons(_, xs)         => filter(xs)(p)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(x => if p(x) then List(x) else Nil)

  def listPlus(l: List[Int], r: List[Int]): List[Int] = (l, r) match
    case (Nil, _)                   => Nil
    case (_, Nil)                   => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, listPlus(xs, ys))

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    (l, r) match
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))

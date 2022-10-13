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

  def init[A](l: List[A]): List[A] = l match
    case Nil          => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))

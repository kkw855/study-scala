package book.red_fp.ch3

sealed trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree:
  def size[A](t: Tree[A]): Int = t match
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_)      => 1

  def maximum(t: Tree[Int]): Int = t match
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v)      => v

  def depth[A](t: Tree[A]): Int = t match
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(_)      => 0

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(value)  => Leaf(f(value))

  def fold[A, B](t: Tree[A])(g: A => B)(f: (B, B) => B): B = t match
    case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
    case Leaf(value)  => g(value)

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)(1 + _ max _)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

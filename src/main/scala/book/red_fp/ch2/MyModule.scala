package book.red_fp.ch2

import scala.annotation.tailrec

//noinspection DuplicatedCode
private object MyModule:
  def abs(a: Int) =
    if a < 0 then -a
    else a

  def formatAbs(a: Int): String =
    val msg = "The absolute value of %d is %d."
    msg.format(a, this.abs(a))

  def factorial(n: Int): Int =
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)

  def formatFactorial(n: Int): String =
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  // Exercise 2.1
  def fibonacci(n: Int): Int =
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if n <= 0 then prev
      else go(n - 1, cur, prev + cur)
    go(n, 0, 1)

  def formatResult(name: String, n: Int, f: Int => Int): String =
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))

  def findFirstMono(ss: Array[String], key: String): Int =
    @tailrec
    def loop(n: Int): Int =
      if n >= ss.length then -1
      else if ss(n) == key then n
      else loop(n + 1)
    loop(0)

  def findFirstPoly[A](ss: Array[A], p: A => Boolean): Int =
    @tailrec
    def loop(n: Int): Int =
      if n >= ss.length then -1
      else if p(ss(n)) then n
      else loop(n + 1)
    loop(0)

  // Exercise 2.2
  def isSorted[A](ss: Array[A], p: (A, A) => Boolean): Boolean =
    @tailrec
    def loop(n: Int): Boolean =
      if n >= ss.length - 1 then true
      else if !p(ss(n), ss(n + 1)) then false
      else loop(n + 1)
    loop(0)

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5
  def compose[A, B, C](f: A => B, g: B => C): A => C =
    a => g(f(a))

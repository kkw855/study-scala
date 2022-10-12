package book.red_fp.ch2

import scala.annotation.tailrec

private object MyModule:
  private def abs(a: Int) =
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

  def fibonacci(n: Int): Int =
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if n <= 0 then prev
      else go(n - 1, cur, prev + cur)
    go(n, 0, 1)

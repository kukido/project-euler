import scala.annotation.tailrec

def fib(limit: Int)(f: Int => Int): Int = {
  @tailrec
  def _fib(a: Int, b: Int, acc: Int): Int = {
    if (a > limit) acc
    else _fib(b, a + b, acc + f(a))
  }
  _fib(1, 2, 0)
}

val f = fib(4000000){ a =>
  if (a % 2 == 0) a else 0
}

println(s"answer: $f")

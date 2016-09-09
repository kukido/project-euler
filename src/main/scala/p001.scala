import scala.annotation.tailrec

def sum(n: Int)(f: Int => Int): Int = {
  @tailrec
  def _sum(k: Int, acc: Int): Int = {
    if (k == 0) acc
    else _sum(k - 1, acc + f(k))
  }
  _sum(n - 1, 0)
}

val s = sum(15){ n =>
  if (n % 3 == 0 || n % 5 == 0) n else 0
}

println(s"answer: $s")
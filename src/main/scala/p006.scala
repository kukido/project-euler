def sum_of_sq(n: Int): Long = {
  def _sum(a: Int, s: Long): Long = {
    if (a > n) s
    else _sum(a + 1, s + a * a)
  }
  _sum(0, 0)
}

def sq_of_sum(n: Int): Long = {
  val s = (1 to n).toList.sum
  s * s
}
val n = 100
val d = sq_of_sum(n) - sum_of_sq(n)

println(s"answer: $d")
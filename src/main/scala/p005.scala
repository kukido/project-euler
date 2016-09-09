def gcd(x: Long, y: Long): Double = {
  def _gcd(a: Long, b: Long, d: Long): (Long, Long) = {
    if (a % 2 == 0 && b % 2 == 0) {
      _gcd(a / 2, b / 2, d + 1)
    } else if (a == b) {
      (a, d)
    } else if (a % 2 == 0) {
      _gcd(a / 2, b, d)
    } else if (b % 2 == 0) {
      _gcd(a, b / 2, d)
    } else if (a > b) {
      _gcd((a - b) / 2, b, d)
    } else {
      _gcd(a, (b - a) / 2, d)
    }
  }

  val (g, d) = _gcd(x, y, 0)

  g * math.pow(2, d)
}

def lcm(n: Long): Long = {
  def _lcm(a: Long, l: Long): Long = {
    if (l > n) {
      a
    } else {
      val b = a * l / gcd(a, l)
      _lcm(b.toLong, l + 1)
    }
  }
  _lcm(1, 1)
}

val d = lcm(30)

println(s"answer: $d")
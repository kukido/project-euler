import scala.annotation.tailrec

// 1. find all abundant
// 2. combination of 2 numbers
// 3. sum of #2
// 4. sum of all to limit 28123
// 5. #4 - #3 = answer

def proper_divisors_sum(x: Int): Int = {
  val l = math.sqrt(x).toInt
  var sum = 1

  for (i <- 2 to l) {
    if (x % i == 0) {
      sum += i
      val d = x / i
      if (d != i) sum += d
    }
  }
  sum
}

def find_abundant(): List[Int] = {
  def _find_abundant(acc: List[Int], n: Int): List[Int] = {
    if (n > 28123) {
      acc
    } else if(is_abundant(n)){
      _find_abundant(n :: acc, n + 1)
    } else {
      _find_abundant(acc, n + 1)
    }
  }
  _find_abundant(List[Int](), 1)
}

def is_abundant(n: Int): Boolean = n < proper_divisors_sum_tr(n)

def find_abundant_pairs(a: List[Int]): List[Int] = {
  val p = for {
    x <- a
    y <- a
    if x + y <= 28123
  } yield {
    x + y
  }
  p.distinct
}

def proper_divisors_sum_tr(x: Int): Int = {
  val l = math.sqrt(x).toInt
  @tailrec
  def _proper_divisors_sum(acc: Int, i: Int): Int = {
    if (i > l) {
      acc
    } else if (x % i == 0) {
      val d = x / i
      if (d != i) {
        _proper_divisors_sum(acc + i + d, i + 1)
      } else {
        _proper_divisors_sum(acc + i, i + 1)
      }
    } else {
      _proper_divisors_sum(acc, i + 1)
    }
  }
  _proper_divisors_sum(1, 2)
}

val abundant = find_abundant()

val pairs = find_abundant_pairs(abundant)

val abundant_pairs_sum = pairs sum

val non_abundant_sum = (1 to 28123 sum) - abundant_pairs_sum

println(s"answer: $non_abundant_sum")
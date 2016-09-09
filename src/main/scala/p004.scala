import scala.annotation.tailrec

def is_palindrome(n: Int): Boolean = {
  @tailrec
  def _is_palindrome(cs: Seq[Char]): Boolean = {
    cs match {
      case Nil => true
      case _ :: Nil => true
      case x if x.head == x.last => _is_palindrome(cs.tail.dropRight(1))
      case _ => false
    }
  }
  _is_palindrome(n.toString.toCharArray)
}

def largest_palindrome(n: Int): Int = {
  val floor = n / 10
  @tailrec
  def _largest_palindrome(a: Int, b: Int, m: Int): Int = {
    if (a == floor && b == floor) {
      m
    } else if (b == floor) {
      _largest_palindrome(a - 1, n, m)
    } else {
      val p = a * b
      val l = if (is_palindrome(p)) {
        math.max(p, m)
      } else m
      _largest_palindrome(a, b - 1, l)
    }
  }
  _largest_palindrome(n, n, 0)
}

val p = largest_palindrome(999)

println(s"answer: $p")
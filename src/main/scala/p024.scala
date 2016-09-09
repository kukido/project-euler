import scala.annotation.tailrec

/*
Condensed mathematical description:

Find largest index i such that array[i − 1] < array[i].

Find largest index j such that j ≥ i and array[j] > array[i − 1].

Swap array[j] and array[i − 1].

Reverse the suffix starting at array[i].
*/

def cutoff(s: Seq[Int]): Int = {
  @tailrec
  def _cutoff(rs: Seq[Int], i: Int): Int = {
    rs match {
      case a :: b :: tail if a <= b => _cutoff(rs.tail, i - 1)
      case _ => i
    }
  }
  _cutoff(s.reverse, s.length - 1)
}

def successor(s: Seq[Int], pi: Int): Int = {
  val p = s(pi)
  @tailrec
  def _successor(rs: Seq[Int], i: Int): Int = {
    rs match {
        // 1 0 2
      case head :: tail if head <= p => _successor(tail, i - 1)
      case _ => i
    }
  }
  _successor(s.reverse, s.length - 1)
}

def swap(s: Seq[Int], a: Int, b: Int): Seq[Int] = {
  val x = s(a)
  val y = s(b)
  s.updated(a, y).updated(b, x)
}

def reverse(s: Seq[Int], c: Int): Seq[Int] = {
  val (head, tail) = s.splitAt(c)
  head ++ tail.reverse
}

def permutate(seq: Seq[Int]): Option[Seq[Int]] = {
  val ci = cutoff(seq)
  val pi = ci - 1
  if (pi < 0) None
  else {
    val si = successor(seq, pi)
    val sw = swap(seq, pi, si)
    val rsw = reverse(sw, ci)
    Some(rsw)
  }
}

@tailrec
def permutation(s: Seq[Int], l: Int): Unit = {
  val pm = permutate(s)
  pm match {
    case Some(p) if l > 1 => permutation(p, l - 1)
    case _ => println(s.mkString)
  }
}

val seq = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
val l = 1000000

permutation(seq, l)
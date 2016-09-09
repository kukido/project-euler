import scala.annotation.tailrec
import scala.math._

def fermat(n: Double): (Double, Double) = {
  assert(n % 2 == 1)
  var a = ceil(sqrt(n))
  var b2 = a * a - n
  while (sqrt(b2) % 1 > 0) {
    a = a + 1
    b2 = a * a - n
  }
  (a - sqrt(b2), a + sqrt(b2))
}

def factorize(n: Double): Double = {
  val factors = scala.collection.mutable.PriorityQueue[Double](n)
  @tailrec
  def _factorize(): Double = {
    val n = factors.dequeue()
    if (n % 2 == 0) {
      factors.enqueue(n / 2)
      _factorize()
    } else {
      val (a, b) = fermat(n)
      if (b != n) {
        factors.enqueue(a, b)
        _factorize()
      } else {
        n
      }
    }
  }
  _factorize()
}

val f = factorize(600851475143L)

println(s"answer: $f")
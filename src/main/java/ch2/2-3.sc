def findFirst[A](as: Array[A], p: A => Boolean): Int = {
  def loop(n: Int): Int =
    if(n >= as.length) -1
    else if(p(as(n))) n
    else loop(n + 1)

  loop(0)
}

val lessThan = new Function2[Int, Int, Boolean] {
  def apply(a: Int, b: Int) = a < b
}

lessThan(10, 20)

// partial appication
def partial1[A, B, C](a: A, f: (A,B) => C): B => C =
  (b: B) => f(a, b)
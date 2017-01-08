// practice 2-2
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def loop(n: Int): Boolean =
    if(n <= 1) true
    else if(ordered(as(n-1), as(n-2))) loop(n-1)
    else false

  loop(as.length)
}

// Verify practice 2-2
isSorted(Array(1, 3, 5, 8, 9), (a: Int,b: Int)=> a >= b)
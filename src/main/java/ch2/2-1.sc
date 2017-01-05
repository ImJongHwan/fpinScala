object MyModule {
  def abs(n: Int): Int =
    if(n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}

def factorial(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if(n <= 0) acc
    else go(n-1, n*acc)

  go(n, 1)
}

// practice 2-1
def fib(n: Int): Int = {
  if(n == 0) 0
  else if (n == 1) 1
  else fib(n-2) + fib(n-1)
}

// verify practice 2-1
fib(6)


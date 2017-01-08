// practice 2-1
def fib(n: Int): Int = {
  def loop(n: Int, b: Int, cc: Int): Int = {
    if(n <= 1) 0
    else if(n == 2) cc
    else loop(n-1, cc, cc+b)
  }

  loop(n, 0, 1)
}

// verify practice 2-1
fib(7)
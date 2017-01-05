// practice 2-1
def fib(n: Int): Int = {
  if(n == 0) 0
  else if (n == 1) 1
  else fib(n-2) + fib(n-1)
}

// verify practice 2-1
fib(6)
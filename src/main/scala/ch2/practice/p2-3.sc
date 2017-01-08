def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

// Verify
val partialCurry = curry((a: Int, b:Int) => a > b)
val doPartial = partialCurry(3)
doPartial(0)
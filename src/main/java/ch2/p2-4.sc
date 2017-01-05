def uncurry[A,B,C](f:A => B => C): (A,B) => C =
  (a: A, b: B) => f(a)(b)

// Verify
val doUncurry = uncurry((a: Int) => (b: Int) => a > b)
doUncurry(3, 2)
def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

// Verify
val doCompose = compose((b: Int) => b * 4, (a: Int) => a + 1)
doCompose(7)

val composing = ((b: Int) => b * 4) compose ((a: Int) => a + 1)
composing(7)

val andThenCompose = ((a: Int) => a + 1) andThen ((b: Int) => b * 4)
andThenCompose(7)
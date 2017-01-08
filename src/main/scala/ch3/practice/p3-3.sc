sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // practice 3.3
  def setHead[A](m: A, ms: List[A]) = ms match {
    case Nil => Cons(m, Nil)
    case Cons(h, t) => Cons(m, t)
  }
}

// verify
val list: List[Int] = List(1, 2, 3, 4, 5)
val verify = List.setHead(6, list)
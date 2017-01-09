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

  // practice 3.2
  def tail[A](as: List[A]) = as match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => t
  }

  // practice 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) =>
      if(f(h)) dropWhile(tail(l), f)
      else l
  }
}

// verify
val list:List[Int] = List(1,2,3,4,5,6)
List.dropWhile(list, (x: Int) => x <= 4)
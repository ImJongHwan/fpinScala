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
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  // practice 3.10
  // ref : https://twitter.github.io/scala_school/ko/collections.html#fold
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // practice 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((x, y) => Cons(f(x), y))

  // practice 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => as
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t) => filter(t)(f)
  }
}

// verify
val list:List[Int] = List(1,2,3,4,5)
List.filter(list)((x) => x%2 == 0)
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

  def makeBigList[A](as: List[A], b: A, n: Int): List[A] =
    if (n <= 0) as
    else makeBigList(Cons(b, as), b, n - 1)


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

  // practice 3.16
  def increment(as: List[Int]): List[Int] = foldRight(as, Nil:List[Int])((x,y) => Cons(x+1, y))
}

// verify
val list:List[Int] = List(1,2,3,4,5)
List.increment(list)
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

  def reverseFold[A](l:List[A]): List[A] = foldLeft(l, Nil:List[A])((y,x) => Cons(x,y))

  // practice 3.13
  def foldRight2[A, B](as:List[A], z: B)(f:(A, B) => B): B = as match {
      case Nil => z
      case _ => foldLeft(reverseFold(as), z)((y, x) => f(x, y))
  }

  def foldRight3[A, B](as:List[A], z:B)(f:(A,B) => B): B =
    foldLeft(as, (b:B) => b)((g, a) => b => g(f(a, b)))(z)
}

val list:List[String] = List("a", "b", "c", "d", "e")
List.foldRight2(list, "")((x,y)=> x + y)
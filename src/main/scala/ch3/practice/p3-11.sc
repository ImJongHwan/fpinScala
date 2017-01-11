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
  def foldLeft[A, B](as: List[A], z: B)(f: (B,A) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // practice 3.11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_+_)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_*_)
  def length2[A](l:List[A]):Int = foldLeft(l, 0)((y,x) => y + 1)
}

// verify
val list:List[Int] = List(1,2,3,4,5,6,7)
val doubleList:List[Double] = List(1.0, 2.0, 3.0, 4.0)
List.sum3(list)
List.product3(doubleList)
List.length2(list)
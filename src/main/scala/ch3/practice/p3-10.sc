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
    if(n <= 0) as
    else makeBigList(Cons(b, as), b, n-1)


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  // practice 3.10
  // ref : https://twitter.github.io/scala_school/ko/collections.html#fold
  def foldLeft[A, B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
}

val list:List[Int] = List.makeBigList(List(1), 2, 1050)
List.foldRight(list, 0)((x,y) => x + y)

val list2:List[Int] = List(1,2,3,4,5,6)
List.foldLeft(list2, 21)((y, x) => y - x)

val stringList1:List[String] = List("a", "b", "c", "d", "e")
List.foldLeft(stringList1, "")((x,y) => x+y)

val stringAr:Array[String] = Array("A", "B", "C", "D", "E")
stringAr.foldLeft("")((x,y) => x + y)

val ar:Array[Int] = Array(1,2,3,4,5,6)
ar.foldLeft(21)((y, x) => y - x)

List.foldRight(list2, 0)((y, x) => y - x)
ar.foldRight(0)((y, x) => y - x)
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
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    println("rec")
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns,0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_*_)
}

val doubleList:List[Double] = List(1.1, 2.5, 0.0, 2.3)
List.product2(doubleList)

// practice 3.7
/*
0.0을 만나도 재귀를 멈추지 않는다.
foldRight(Cons(1.1, Cons(2.5, Cons(0.0, Cons(2.3, Nil)))), 1.0)((x, y) => x * y)
1.1 * foldRight(Cons(2.5, Cons(0.0, Cons(2.3, Nil))), 1.0)((x, y => x * y)
1.1 * 2.5 * foldRight(Cons(0.0, Cons(2.3, Nil)), 1.0)((x, y => x * y)
1.1 * 2.5 * 0.0 * foldRight(Cons(2.3, Nil), 1.0)((x, y => x * y)
1.1 * 2.5 * 0.0 * 2.3 * foldRight(Nil, 1.0)((x, y => x * y)
1.1 * 2.5 * 0.0 * 2.3 * 1.0
0.0
 */
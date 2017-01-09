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
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // practice 3.6
  /*
  상수 시간으로 구현할 수 없는 이유 :
  List의 구성이 Cons(head Element, tail List)로 구성 되어 있기 때문에
  List의 마지막에 도달하려면 tail의 List를 모두 방문해야 한다.
   */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
}

// Verify
val list:List[Int] = List(1, 2, 3, 4, 5, 6, 7)
List.init(list)
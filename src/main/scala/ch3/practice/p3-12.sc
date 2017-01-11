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
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //practice 3.12
  def reverse[A](l:List[A]): List[A] = {
    def loop(res:List[A], as:List[A]): List[A] = as match {
      case Nil => res
      case Cons(h, t) => loop(Cons(h, res), t)
    }
    loop(Nil:List[A], l)
  }

  def reverseFold[A](l:List[A]): List[A] = foldLeft(l, Nil:List[A])((y,x) => Cons(x,y))
}

// verify
val list:List[Int] = List(1,2,3,4,5,6,7)
List.reverse(list)

List.reverseFold(list)
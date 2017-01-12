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

  // practice 3.14
  def append[A](fs: List[A], bs: List[A]): List[A] = foldRight(fs, bs)((x, y) => Cons(x, y))

  // practice 3.15
  def flatten[A](fs: List[List[A]]): List[A] = foldRight(fs, Nil: List[A])((x, y) => List.append(x, y))

  // practice 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))

  // practice 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => as
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t) => filter(t)(f)
  }

  // practice 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  foldRight(as, Nil: List[B])((x, y) => List.append(f(x), y))

  // practice 3.24
  def hasSubsequence[A](sup:List[A], sub:List[A]):Boolean = {
    def loop[A](as: List[A], bs: List[A]): Boolean =
      (as, bs) match {
        case (Cons(x, xt), Cons(y, yt)) if x == y => loop(xt, yt)
        case (_, Nil) => true
        case _ => false
      }
    sup match {
      case Cons(h, t) if loop(Cons(h, t), sub) => true
      case Cons(h, t) => hasSubsequence(t, sub)
      case _ => false
    }
  }
}

// verify
val supList:List[Int] = List(1,2,3,4)
val subList:List[Int] = List(1,2)

List.hasSubsequence(supList, subList)
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
}

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))

ex1.toString
ex2.toString
ex3.toString

List(1, 2, 3) match {case Cons(h, _) => h}
Cons(1, Cons(2, Cons(3, Nil))) match {case Cons(h, _) => h}


// page 47
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
  l match {
    case Cons(h,t) if f(h) => dropWhile(t)(f)
    case _ => l
  }
}

val xs: List[Int] = List(1,2,3,4,5)
val ex_1 = dropWhile(xs)(x => x < 4)
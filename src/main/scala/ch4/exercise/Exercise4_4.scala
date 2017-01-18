package ch4.exercise

/**
  * Created by hwan on 2017. 1. 19..
  */
object Exercise4_4 {
  def main(args:Array[String]):Unit = {
    val list:List[Option[Int]] = List(Some(1), Some(2), Some(3), None, Some(4))

    println(sequence(list))
  }
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a.flatMap((a) => b.map((b) => f(a,b)))
  }
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
  }
}

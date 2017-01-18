package ch4.exercise

/**
  * Created by hwan on 2017. 1. 19..
  */
object Exercise4_3 {
  def main(args: Array[String]): Unit = {
    val opt1:Option[Int] = Some(1)
    val opt2:Option[String] = Some("2")

    println(map2(opt1, opt2)((x, y) => 3.0 + x))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a.flatMap((a) => b.map((b) => f(a,b)))
  }
}

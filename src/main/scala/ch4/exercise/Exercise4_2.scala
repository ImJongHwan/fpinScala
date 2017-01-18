package ch4.exercise

/**
  * Created by hwan on 2017. 1. 18..
  */
object Exercise4_2 {
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]):Option[Double] = {
    mean(xs) flatMap((m) => mean(xs.map((a) => math.pow(a-m, 2))))
  }

  def main(args: Array[String]):Unit = {
    val seq:Seq[Double] = Seq()
    println(variance(seq))
  }
}

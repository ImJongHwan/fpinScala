package ch4.exercise

/**
  * Created by hwan on 2017. 1. 18..
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f) getOrElse None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map((a) => Some(a)) getOrElse ob
  }
  def filter(f: A => Boolean): Option[A] = {
    flatMap((a) => if(f(a)) this else None)
  }
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Exercise4_1 {
  def main(args: Array[String]): Unit = {
    val some:Option[Int] = Some(4)
    val none:Option[Int] = None

    println("some map : " + some.map((x) => x+1))
    println("some flatMap : " + some.flatMap((x) => Some(x+2)))
    println("some getOrElse : " + some.getOrElse(-1))
    println("some orElse : " + some.orElse(Some(-1)))
    println("some filter : " + some.filter((x) => x == 4))

    println()

    println("none map : " + none.map((x) => x+1))
    println("none flatMap : " + none.flatMap((x) => Some(x+2)))
    println("none getOrElse : " + none.getOrElse(-1))
    println("none orElse : " + none.orElse(Some(-1)))
    println("none filter : " + none.filter((x) => x == 4))
  }
}
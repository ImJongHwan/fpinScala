sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // practice 3.25
  def size[A](t: Tree[A]): Int =
  t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(x) => 1
  }

  // practice 3.26
  def maximum(t: Tree[Int]): Int = {
    def branchMax(a: Tree[Int], max: Int): Int =
      a match {
        case Branch(l, r) => branchMax(l, max) max branchMax(r, max)
        case Leaf(x) => max max x
      }
    branchMax(t, Int.MinValue)
  }
}

// verify
val tree:Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Leaf(5)))
Tree.maximum(tree)
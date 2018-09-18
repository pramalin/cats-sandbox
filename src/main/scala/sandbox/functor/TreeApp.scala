package sandbox.functor

import cats.syntax.functor._
import FunctorImplicits._

object TreeApp extends App {
  val leaf1 = Leaf[Int](1)
  val leaf2 = Leaf[Int](2)

  val t1 = Branch[Int](leaf1, leaf2)

  println(Tree.leaf(100).map(_ * 2))
  println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2))

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  val f = (v: Int) => v.toString + "foo"
  val f2 = (v: String) => v + "bar"
  tree.map(f)
  println(tree)
  println(tree.map(f).map(f2))
}
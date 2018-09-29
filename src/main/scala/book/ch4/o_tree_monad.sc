package book.ch4
import sandbox.monads.TreeMonad._

object o_tree_monad {
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  branch(leaf(100), leaf(200)).
    flatMap(x => branch(leaf(x - 1), leaf(x + 1)))//> res0: sandbox.monads.TreeMonad.Tree[Int] = Branch(Branch(Leaf(99),Leaf(101))
                                                  //| ,Branch(Leaf(199),Leaf(201)))

  // We can also transform Trees using for comprehensions:
  for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c                                       //> res1: sandbox.monads.TreeMonad.Tree[Int] = Branch(Branch(Branch(Leaf(89),Lea
                                                  //| f(91)),Branch(Leaf(109),Leaf(111))),Branch(Branch(Leaf(189),Leaf(191)),Branc
                                                  //| h(Leaf(209),Leaf(211))))
}
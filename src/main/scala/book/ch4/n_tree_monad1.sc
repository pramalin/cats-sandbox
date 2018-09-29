package book.ch4
import sandbox.monads.TreeMonad1._

object n_tree_monad1 {
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  branch(leaf(100), leaf(200)).
    flatMap(x => branch(leaf(x - 1), leaf(x + 1)))//> java.lang.NullPointerException
                                                  //| 	at cats.FlatMap$Ops.flatMap(FlatMap.scala:21)
                                                  //| 	at cats.FlatMap$Ops.flatMap$(FlatMap.scala:21)
                                                  //| 	at cats.FlatMap$ToFlatMapOps$$anon$2.flatMap(FlatMap.scala:21)
                                                  //| 	at book.ch4.n_tree_monad1$.$anonfun$main$1(book.ch4.n_tree_monad1.scala:
                                                  //| 9)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$anonfun$$ex
                                                  //| ecute$1(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:76)
                                                  //| 	at book.ch4.n_tree_monad1$.main(book.ch4.n_tree_monad1.scala:6)
                                                  //| 	at book.ch4.n_tree_monad1.main(book.ch4.n_tree_monad1.scala)

  // We can also transform Trees using for comprehensions:
  for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c

}
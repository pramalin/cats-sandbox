package book.ch4
import sandbox.monads.IdMonadInstances._

object excercises {

  pure(123)                                       //> res0: cats.Id[Int] = 123
  map(123)(_ * 2)                                 //> res1: cats.Id[Int] = 246
  flatMap(123)(_ * 2)                             //> res2: cats.Id[Int] = 246
}
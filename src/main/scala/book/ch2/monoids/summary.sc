package book.ch2.monoids

import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.semigroup._ // for |+|

object summary {
  Monoid[String].combine("Hi ", "there")

  "Scala" |+| " with " |+| "Cats"

  import cats.instances.int._ // for Monoid
  import cats.instances.option._ // for Monoid

  Option(1) |+| Option(2)

  import cats.instances.map._ // for Monoid

  val map1 = Map("a" -> 1, "b" -> 2)
  val map2 = Map("b" -> 3, "d" -> 4)
  map1 |+| map2

  import cats.instances.tuple._ // for Monoid

  val tuple1 = ("hello", 123)
  val tuple2 = ("world", 321)
  tuple1 |+| tuple2


  def addAll[A](values: List[A])
    (implicit monoid: Monoid[A]): A =
       values.foldRight(monoid.empty)(_ |+| _)
  
  addAll(List(1, 2, 3))
  addAll(List(None, Some(1), Some(2)))

}
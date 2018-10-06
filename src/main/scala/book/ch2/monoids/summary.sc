package book.ch2.monoids

import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.semigroup._ // for |+|

object summary {
  Monoid[String].combine("Hi ", "there")          //> res0: String = Hi there

  "Scala" |+| " with " |+| "Cats"                 //> res1: String = Scala with Cats

  import cats.instances.int._ // for Monoid
  import cats.instances.option._ // for Monoid

  Option(1) |+| Option(2)                         //> res2: Option[Int] = Some(3)

  import cats.instances.map._ // for Monoid

  val map1 = Map("a" -> 1, "b" -> 2)              //> map1  : scala.collection.immutable.Map[String,Int] = Map(a -> 1, b -> 2)
  val map2 = Map("b" -> 3, "d" -> 4)              //> map2  : scala.collection.immutable.Map[String,Int] = Map(b -> 3, d -> 4)
  map1 |+| map2                                   //> res3: Map[String,Int] = Map(b -> 5, d -> 4, a -> 1)

  import cats.instances.tuple._ // for Monoid

  val tuple1 = ("hello", 123)                     //> tuple1  : (String, Int) = (hello,123)
  val tuple2 = ("world", 321)                     //> tuple2  : (String, Int) = (world,321)
  tuple1 |+| tuple2                               //> res4: (String, Int) = (helloworld,444)


  def addAll[A](values: List[A])
    (implicit monoid: Monoid[A]): A =
       values.foldRight(monoid.empty)(_ |+| _)    //> addAll: [A](values: List[A])(implicit monoid: cats.Monoid[A])A
  
  addAll(List(1, 2, 3))
  addAll(List(None, Some(1), Some(2)))

}
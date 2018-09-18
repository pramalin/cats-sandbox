package sandbox.monoid
import cats.Monoid
import cats.syntax.option._
import cats.syntax.semigroup._

object SuperAdder {
  // No monoids, replicated code
  def add(items: List[Int]) = {
    items.sum
  }
  def addOption(items: List[Option[Int]]) = {
    items.foldLeft(0.some)((z, v) => z.flatMap(zv => v.map(_ + zv)))
  }

  def addMonoid[A](v: List[A])(implicit ev: Monoid[A]): A = {
    v.foldLeft(ev.empty)(_ |+| _)
  }
}

object SuperAdderApp extends App {
  import SuperAdder._
  import cats.instances.int._
  import cats.syntax.option._
  import MonoidInstances._
  import cats.instances.option._

  val inferedTypeWithSmart: List[Option[Int]] = List(1.some,2.some,3.some,4.some)
  val inferedTypeWithNormal: List[Some[Int]] = List(Some(1), Some(2), Some(3), Some(4))

  println(s"the added monoid for ints is ${addMonoid(List(1,2,3,4))}")
  println(s"the added monoid for options is ${addMonoid(inferedTypeWithSmart)}")

  val orderList = List(Order(10, 2), Order(20, 3), Order(40, 1))
  println(s"the added monoid for orders is ${addMonoid(orderList)}")
}
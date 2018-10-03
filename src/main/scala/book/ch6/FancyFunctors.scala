package book.ch6

import cats.Monoid
import cats.instances.int._ // for Monoid
import cats.instances.list._ // for Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.apply._ // for imapN
import cats.syntax.semigroup._ // for |+|
import cats.instances.invariant._ // for  catsSemigroupalForMonoid: InvariantSemigroupal[Monoid] 

object FancyFunctors extends App {

  /*
  val tupleToCat: (String, Int, List[String]) => Cat2 =
    Cat2.apply _

  val catToTuple: Cat2 => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  */

  case class Cat2(name: String, yearOfBirth: Int, favoriteFoods: List[String])
  implicit val catMonoid: Monoid[Cat2] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(Cat2.apply)(c => Cat2.unapply(c).get)

  val garfield = Cat2("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat2("Heathcliff", 1988, List("Junk Food"))
  
  println(garfield |+| heathcliff)
}
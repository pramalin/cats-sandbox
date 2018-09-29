package book.ch6

object b_semigroupal_apply {
  /*
		6.2.1 Fancy Functors and Apply Syntax
		Apply syntax also has contramapN and imapN methods that accept Contravariant
		and Invariant functors. For example, we can combine Monoids using Invariant.
		Here’s an example:

		requires cats >= 1.0.1
	*/

  import cats.Monoid
  import cats.instances.boolean._ // for Monoid
  import cats.instances.int._ // for Monoid
  import cats.instances.list._ // for Monoid
  import cats.instances.string._ // for Monoid
  import cats.syntax.apply._ // for imapN
  import cats.syntax.semigroup._ // for |+|
  import cats.instances.invariant._ // for  catsSemigroupalForMonoid: InvariantSemigroupal[Monoid]

  case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat =
    Cat.apply _                                   //> tupleToCat  : (String, Int, List[String]) => book.ch6.b_semigroupal_apply.Ca
                                                  //| t = book.ch6.b_semigroupal_apply$$$Lambda$3/1851691492@497470ed

  val catToTuple: Cat => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)
                                                  //> catToTuple  : book.ch6.b_semigroupal_apply.Cat => (String, Int, List[String]
                                                  //| ) = book.ch6.b_semigroupal_apply$$$Lambda$9/2114889273@3d24753a

  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]).imapN(tupleToCat)(catToTuple)
                                                  //> catMonoid  : cats.Monoid[book.ch6.b_semigroupal_apply.Cat] = cats.instances
                                                  //| .InvariantMonoidalInstances$$anon$3$$anon$5@55d56113
  /* Stack overflow version is cleaner:
  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]).imapN(Cat.apply)(c => Cat.unapply(c).get)
  */

  /*
		Our Monoid allows us to create “empty” Cats, and add Cats together using
		the syntax from Chapter 2:
	*/

  val garfield = Cat("Garfield", 1978, List("Lasagne"))
                                                  //> garfield  : book.ch6.b_semigroupal_apply.Cat = Cat(Garfield,1978,List(Lasag
                                                  //| ne))
  val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
                                                  //> heathcliff  : book.ch6.b_semigroupal_apply.Cat = Cat(Heathcliff,1988,List(J
                                                  //| unk Food))

  garfield |+| heathcliff                         //> res0: book.ch6.b_semigroupal_apply.Cat = Cat(GarfieldHeathcliff,3966,List(L
                                                  //| asagne, Junk Food))

}
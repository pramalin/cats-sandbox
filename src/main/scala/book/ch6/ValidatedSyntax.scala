package book.ch6

import cats.data.Validated
import cats.Semigroupal
import cats.instances.string._ // for Semigroup
import cats.syntax.validated._

object ValidatedSyntax extends App {
  /*
		6.4.2 Combining Instances of Validated
		We can combine instances of Validated using any of the methods or syntax
		described for Semigroupal above.
		All of these techniques require an instance of Semigroupal to be in scope. As
		with Either, we need to fix the error type to create a type constructor with
		the correct number of parameters for Semigroupal:
	*/

  type AllErrorsOr[A] = Validated[String, A]
  /*
		Validated accumulates errors using a Semigroup, so we need one of those
		in scope to summon the Semigroupal. If no Semigroup is visible at the call
		site, we get an annoyingly unhelpful compilation error:
	*/

  // Once we import a Semigroup for the error type, everything works as expected:

  Semigroupal[AllErrorsOr]

  /*
		As long as the compiler has all the implicits in scope to summon a Semigroupal
		of the correct type, we can use apply syntax or any of the other
		Semigroupal methods to accumulate errors as we like:
	*/

//  ("Error 1".invalid[Int],
//   "Error 2".invalid[Int]).tupled

  /*
		As you can see, String isn’t an ideal type for accumulating errors. We commonly
		use Lists or Vectors instead:
	*/

//  ( Vector(404).invalid[Int],
//    Vector(500).invalid[Int]).tupled

  /*
		The cats.data package also provides the NonEmptyList and NonEmptyVector
		types that prevent us failing without at least one error:
	*/

//  ( NonEmptyVector.of("Error 1").invalid[Int],
//    NonEmptyVector.of("Error 2").invalid[Int]).tupled

}
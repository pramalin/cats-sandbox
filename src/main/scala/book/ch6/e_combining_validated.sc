package book.ch6

object e_combining_validated {
  /*
		6.4.2 Combining Instances of Validated
		We can combine instances of Validated using any of the methods or syntax
		described for Semigroupal above.
		All of these techniques require an instance of Semigroupal to be in scope. As
		with Either, we need to fix the error type to create a type constructor with
		the correct number of parameters for Semigroupal:
	*/

  import cats.Semigroupal
  import cats.data.Validated

  type AllErrorsOr[A] = Validated[String, A]
  /*
		Validated accumulates errors using a Semigroup, so we need one of those
		in scope to summon the Semigroupal. If no Semigroup is visible at the call
		site, we get an annoyingly unhelpful compilation error:
	*/

  //  Semigroupal[AllErrorsOr]
  // <console>:28: error: could not find implicit value for parameter
  // instance: cats.Semigroupal[AllErrorsOr]
  // Semigroupal[AllErrorsOr]
  // ^

  // Once we import a Semigroup for the error type, everything works as expected:

  import cats.instances.string._ // for Semigroup
  Semigroupal[AllErrorsOr]                        //> res0: cats.Semigroupal[book.ch6.e_combining_validated.AllErrorsOr] = cats.d
                                                  //| ata.ValidatedInstances$$anon$1@6d9c638

  /*
		As long as the compiler has all the implicits in scope to summon a Semigroupal
		of the correct type, we can use apply syntax or any of the other
		Semigroupal methods to accumulate errors as we like:
	*/

  import cats.syntax.apply._ // for tupled
  import cats.syntax.validated._

  ( "Error 1".invalid[Int],
    "Error 2".invalid[Int]).tupled                //> res1: cats.data.Validated[String,(Int, Int)] = Invalid(Error 1Error 2)

  /*
		As you can see, String isn’t an ideal type for accumulating errors. We commonly
		use Lists or Vectors instead:
	*/
  import cats.instances.vector._ // for Semigroupal

    ( Vector(404).invalid[Int],
      Vector(500).invalid[Int]).tupled            //> res2: cats.data.Validated[scala.collection.immutable.Vector[Int],(Int, Int)
                                                  //| ] = Invalid(Vector(404, 500))

  /*
		The cats.data package also provides the NonEmptyList and NonEmptyVector
		types that prevent us failing without at least one error:
	*/

  import cats.data.NonEmptyVector

    ( NonEmptyVector.of("Error 1").invalid[Int],
      NonEmptyVector.of("Error 2").invalid[Int]).tupled
                                                  //> res3: cats.data.Validated[cats.data.NonEmptyVector[String],(Int, Int)] = In
                                                  //| valid(NonEmptyVector(Error 1, Error 2))

}
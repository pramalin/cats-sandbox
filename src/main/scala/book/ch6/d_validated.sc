package book.ch6

object d_validated {
  /*
		6.4 Validated

		By now we are familiar with the fail-fast error handling behaviour of Either.
		Furthermore, because Either is a monad, we know that the semantics of
		product are the same as those for flatMap. In fact, it is impossible for us
		to design a monadic data type that implements error accumulating semantics
		without breaking the consistency of these two methods.
		Fortunately, Cats provides a data type called Validated that has an instance
		of Semigroupal but no instance of Monad. The implementation of product
		is therefore free to accumulate errors:
	*/

  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._ // for Monoid
  type AllErrorsOr[A] = Validated[List[String], A]

  Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2")))           //> res0: book.ch6.d_validated.AllErrorsOr[(Nothing, Nothing)] = Invalid(List(Er
                                                  //| ror 1, Error 2))

  /*
		Validated complements Either nicely. Between the two we have support
		for both of the common types of error handling: fail-fast and accumulating.

		6.4.1 Creating Instances of Validated
		Validated has two subtypes, Validated.Valid and Validated.Invalid,
		that correspond loosely to Right and Left. There are a lot of ways to create
		instances of these types. We can create them directly using their apply
		methods:
	*/

  val v = Validated.Valid(123)                    //> v  : cats.data.Validated.Valid[Int] = Valid(123)
  val i = Validated.Invalid(List("Badness"))      //> i  : cats.data.Validated.Invalid[List[String]] = Invalid(List(Badness))
  
  /*
		However, it is often easier to use the valid and invalid smart constructors,
		which widen the return type to Validated:
	*/

  val v2 = Validated.valid[List[String], Int](123)//> v2  : cats.data.Validated[List[String],Int] = Valid(123)
  val i2 = Validated.invalid[List[String], Int](List("Badness"))
                                                  //> i2  : cats.data.Validated[List[String],Int] = Invalid(List(Badness))
  /*
		As a third option we can import the valid and invalid extension methods
		from cats.syntax.validated:
	*/

  import cats.syntax.validated._ // for valid and invalid
  123.valid[List[String]]                         //> res1: cats.data.Validated[List[String],Int] = Valid(123)

  List("Badness").invalid[Int]                    //> res2: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

  /*
		As a fourth option we can use pure and raiseError from
		cats.syntax.applicative and cats.syntax.applicativeError
		respectively:
	*/

  import cats.syntax.applicative._ // for pure
  import cats.syntax.applicativeError._ // for raiseError
  type ErrorsOr[A] = Validated[List[String], A]
  123.pure[ErrorsOr]                              //> res3: book.ch6.d_validated.ErrorsOr[Int] = Valid(123)

  List("Badness").raiseError[ErrorsOr, Int]       //> res4: book.ch6.d_validated.ErrorsOr[Int] = Invalid(List(Badness))

  /*
		Finally, there are helper methods to create instances of Validated from different
		sources. We can create them from Exceptions, as well as instances of
		Try, Either, and Option:
	*/

  Validated.catchOnly[NumberFormatException]("foo".toInt)
                                                  //> res5: cats.data.Validated[NumberFormatException,Int] = Invalid(java.lang.Nu
                                                  //| mberFormatException: For input string: "foo")

  Validated.catchNonFatal(sys.error("Badness"))   //> res6: cats.data.Validated[Throwable,Nothing] = Invalid(java.lang.RuntimeExc
                                                  //| eption: Badness)

  Validated.fromTry(scala.util.Try("foo".toInt))  //> res7: cats.data.Validated[Throwable,Int] = Invalid(java.lang.NumberFormatEx
                                                  //| ception: For input string: "foo")

  Validated.fromEither[String, Int](Left("Badness"))
                                                  //> res8: cats.data.Validated[String,Int] = Invalid(Badness)

  Validated.fromOption[String, Int](None, "Badness")
                                                  //> res9: cats.data.Validated[String,Int] = Invalid(Badness)
}
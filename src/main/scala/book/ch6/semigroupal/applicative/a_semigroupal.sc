package book.ch6.semigroupal.applicative

object a_semigroupal {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  /*
		Chapter 6
		
		Semigroupal and Applicative
		In previous chapters we saw how functors and monads let us sequence opera-
		tions using map and flatMap. While functors and monads are both immensely
		useful abstractions, there are certain types of program flow that they cannot
		represent.
		
		One such example is form validation. When we validate a form we want to
		return all the errors to the user, not stop on the first error we encounter. If we
		model this with a monad like Either, we fail fast and lose errors. For example,
		the code below fails on the first call to parseInt and doesn’t go any further:
	*/
		import cats.syntax.either._ // for catchOnly

		def parseInt(str: String): Either[String, Int] =
			Either.catchOnly[NumberFormatException](str.toInt).
				leftMap(_ => s"Couldn't read $str")
                                                  //> parseInt: (str: String)Either[String,Int]

		for {
			a <- parseInt("a")
			b <- parseInt("b")
			c <- parseInt("c")
		} yield (a + b + c)               //> res0: scala.util.Either[String,Int] = Left(Couldn't read a)

	/*
		Another example is the concurrent evaluation of Futures. If we have several
		long-running independent tasks, it makes sense to execute them concurrently.
		However, monadic comprehension only allows us to run them in sequence.
		map and flatMap aren’t quite capable of capturing what we want because
		they make the assumption that each computation is dependent on the previous
		one:

					// context2 is dependent on value1:
					context1.flatMap(value1 => context2)

		The calls to parseInt and Future.apply above are independent of one another,
		but map and flatMap can’t exploit this. We need a weaker construct—
		one that doesn’t guarantee sequencing—to achieve the result we want. In this
		chapter we will look at two type classes that support this pattern:

			• Semigroupal encompasses the notion of composing pairs of contexts.
			Cats provides a cats.syntax.apply module that makes use of Semigroupal
			and Functor to allow users to sequence functions with mul-
			tiple arguments.
	
			• Applicative extends Semigroupal and Functor. It provides a way
			of applying functions to parameters within a context. Applicative is
			the source of the pure method we introduced in Chapter 4.

		Applicatives are often formulated in terms of function application, instead of
		the semigroupal formulation that is emphasised in Cats. This alternative formula
		tion provides a link to other libraries and languages such as Scalaz and
		Haskell. We’ll take a look at different formulations of Applicative, as well as
		the relationships between Semigroupal, Functor, Applicative, and Monad,
		towards the end of the chapter.

		6.1 Semigroupal

		cats.Semigroupal is a type class that allows us to combine contexts¹.
		[¹It is also the winner of Underscore’s 2017 award for the most difficult functional programming
		term to work into a coherent English sentence.]
		
	 	If we have two objects of type F[A] and F[B], a Semigroupal[F] allows us to
		combine them to form an F[(A, B)]. Its definition in Cats is:
		
					trait Semigroupal[F[_]] {
					def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
					}

		As we discussed at the beginning of this chapter, the parameters fa and fb
		are independent of one another: we can compute them in either order before
		passing them to product. This is in contrast to flatMap, which imposes
		a strict order on its parameters. This gives us more freedom when defining
		instances of Semigroupal than we get when defining Monads.

		6.1.1 Joining Two Contexts

		While Semigroup allows us to join values, Semigroupal allows us to join contexts.
		Let’s join some Options as an example:
	*/
	
		import cats.Semigroupal
		import cats.instances.option._ // for Semigroupal

		Semigroupal[Option].product(Some(123), Some("abc"))
                                                  //> res1: Option[(Int, String)] = Some((123,abc))

	/*
		If both parameters are instances of Some, we end up with a tuple of the values
		within. If either parameter evaluates to None, the entire result is None:
	*/
	
		Semigroupal[Option].product(None, Some("abc"))
                                                  //> res2: Option[(Nothing, String)] = None

		Semigroupal[Option].product(Some(123), None)
                                                  //> res3: Option[(Int, Nothing)] = None

	/*
		6.1.2 Joining Three or More Contexts

		The companion object for Semigroupal defines a set of methods on top of
		product. For example, the methods tuple2 through tuple22 generalise
		product to different arities:
	*/
	
		import cats.instances.option._ // for Semigroupal

		Semigroupal.tuple3(Option(1), Option(2), Option(3))
                                                  //> res4: Option[(Int, Int, Int)] = Some((1,2,3))

		Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
                                                  //> res5: Option[(Int, Int, Int)] = None

	/*
		The methods map2 through map22 apply a user-specified function to the values
		inside 2 to 22 contexts:
	*/
	
		Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
                                                  //> res6: Option[Int] = Some(6)

		Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
                                                  //> res7: Option[Int] = None

	/*
		There are also methods contramap2 through contramap22 and imap2
		through imap22, that require instances of Contravariant and Invariant
		respectively.

		6.2 Apply Syntax

		Cats provides a convenient apply syntax that provides a shorthand for the
		methods described above. We import the syntax from cats.syntax.apply.
		Here’s an example:
	*/
	
		import cats.instances.option._ // for Semigroupal
		import cats.syntax.apply._ // for tupled and mapN

	/*
		The tupled method is implicitly added to the tuple of Options. It uses the
		Semigroupal for Option to zip the values inside the Options, creating a single
		Option of a tuple:
	*/
		(Option(123), Option("abc")).tupled
                                                  //> res8: Option[(Int, String)] = Some((123,abc))

	/*
		We can use the same trick on tuples of up to 22 values. Cats defines a separate
		tupled method for each arity:
	*/
	
		(Option(123), Option("abc"), Option(true)).tupled
                                                  //> res9: Option[(Int, String, Boolean)] = Some((123,abc,true))

	/*
		In addition to tupled, Cats’ apply syntax provides a method called mapN that
		accepts an implicit Functor and a function of the correct arity to combine the
		values:
	*/
		case class Cat(name: String, born: Int, color: String)

		(
			Option("Garfield"),
			Option(1978),
			Option("Orange & black")
		).mapN(Cat.apply)                 //> res10: Option[book.ch6.semigroupal.applicative.a_semigroupal.Cat] = Some(Ca
                                                  //| t(Garfield,1978,Orange & black))

	/*
		Internally mapN uses the Semigroupal to extract the values from the Option
		and the Functor to apply the values to the function.
		It’s nice to see that this syntax is type checked. If we supply a function that
		accepts the wrong number or types of parameters, we get a compile error:
	*/

		val add: (Int, Int) => Int = (a, b) => a + b
                                                  //> add  : (Int, Int) => Int = book.ch6.semigroupal.applicative.a_semigroupal$$
                                                  //| $Lambda$28/1597249648@553f17c

		// (Option(1), Option(2), Option(3)).mapN(add)
		// <console>:27: error: type mismatch;
		// found : (Int, Int) => Int
		// required: (Int, Int, Int) => ?
		// (Option(1), Option(2), Option(3)).mapN(add)
		// ^
		// (Option("cats"), Option(true)).mapN(add)
		// <console>:27: error: type mismatch;

		// found : (Int, Int) => Int
		// required: (String, Boolean) => ?
		// (Option("cats"), Option(true)).mapN(add)
		// ^

	/*
		6.2.1 Fancy Functors and Apply Syntax

		Apply syntax also has contramapN and imapN methods that accept Contravariant
		and Invariant functors. For example, we can combine Monoids using Invariant.
		Here’s an example:
	*/
	
		import cats.Monoid
		import cats.instances.boolean._ // for Monoid
		import cats.instances.int._ // for Monoid
		import cats.instances.list._ // for Monoid
		import cats.instances.string._ // for Monoid
		import cats.syntax.apply._ // for imapN
    import cats.instances.invariant._ // for  catsSemigroupalForMonoid: InvariantSemigroupal[Monoid]

		case class Cat2(
			name: String,
			yearOfBirth: Int,
			favoriteFoods: List[String]
		)

		val tupleToCat: (String, Int, List[String]) => Cat2 =
			Cat2.apply _              //> tupleToCat  : (String, Int, List[String]) => book.ch6.semigroupal.applicati
                                                  //| ve.a_semigroupal.Cat2 = book.ch6.semigroupal.applicative.a_semigroupal$$$La
                                                  //| mbda$29/1486371051@42dafa95

		val catToTuple: Cat2 => (String, Int, List[String]) =
			cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)
                                                  //> catToTuple  : book.ch6.semigroupal.applicative.a_semigroupal.Cat2 => (Strin
                                                  //| g, Int, List[String]) = book.ch6.semigroupal.applicative.a_semigroupal$$$La
                                                  //| mbda$30/1694556038@402a079c

		implicit val catMonoid: Monoid[Cat2] = (
			Monoid[String],
			Monoid[Int],
			Monoid[List[String]]
		).imapN(tupleToCat)(catToTuple)   //> catMonoid  : cats.Monoid[book.ch6.semigroupal.applicative.a_semigroupal.Cat
                                                  //| 2] = cats.instances.InvariantMonoidalInstances$$anon$3$$anon$5@2a17b7b6

	/*
		Our Monoid allows us to create “empty” Cats, and add Cats together using
		the syntax from Chapter 2:
	*/
	
		import cats.syntax.semigroup._ // for |+|
		val garfield = Cat2("Garfield", 1978, List("Lasagne"))
                                                  //> garfield  : book.ch6.semigroupal.applicative.a_semigroupal.Cat2 = Cat2(Garf
                                                  //| ield,1978,List(Lasagne))
		val heathcliff = Cat2("Heathcliff", 1988, List("Junk Food"))
                                                  //> heathcliff  : book.ch6.semigroupal.applicative.a_semigroupal.Cat2 = Cat2(He
                                                  //| athcliff,1988,List(Junk Food))
		garfield |+| heathcliff           //> res11: book.ch6.semigroupal.applicative.a_semigroupal.Cat2 = Cat2(GarfieldH
                                                  //| eathcliff,3966,List(Lasagne, Junk Food))

	/*
		6.3 Semigroupal Applied to Different Types

		Semigroupal doesn’t always provide the behaviour we expect, particularly
		for types that also have instances of Monad. We have seen the behaviour of
		the Semigroupal for Option. Let’s look at some examples for other types.

		Future

		The semantics for Future provide parallel as opposed to sequential execution:
	*/
	
		import cats.Semigroupal
		import cats.instances.future._ // for Semigroupal
		import scala.concurrent._
		import scala.concurrent.duration._
		import scala.concurrent.ExecutionContext.Implicits.global
		import scala.language.higherKinds

		val futurePair = Semigroupal[Future].
			product(Future("Hello"), Future(123))
                                                  //> futurePair  : scala.concurrent.Future[(String, Int)] = Future(<not complete
                                                  //| d>)

		Await.result(futurePair, 1.second)//> res12: (String, Int) = (Hello,123)

	/*
		The two Futures start executing the moment we create them, so they are
		already calculating results by the time we call product. We can use apply
		syntax to zip fixed numbers of Futures:
	*/
	
		import cats.syntax.apply._ // for mapN

		case class Cat3(
			name: String,
			yearOfBirth: Int,
			favoriteFoods: List[String]
		)

		val futureCat = (
			Future("Garfield"),
			Future(1978),
			Future(List("Lasagne"))
		).mapN(Cat3.apply)                //> futureCat  : scala.concurrent.Future[book.ch6.semigroupal.applicative.a_sem
                                                  //| igroupal.Cat3] = Future(<not completed>)

		Await.result(futureCat, 1.second) //> res13: book.ch6.semigroupal.applicative.a_semigroupal.Cat3 = Cat3(Garfield,
                                                  //| 1978,List(Lasagne))

	/*
		List

		Combining Lists with Semigroupal produces some potentially unexpected
		results. We might expect code like the following to zip the lists, but we actually
		get the cartesian product of their elements:
	*/
	
		import cats.Semigroupal
		import cats.instances.list._ // for Semigroupal

		Semigroupal[List].product(List(1, 2), List(3, 4))
                                                  //> res14: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))

	/*
		This is perhaps surprising. Zipping lists tends to be a more common operation.
		We’ll see why we get this behaviour in a moment.

		Either

		We opened this chapter with a discussion of fail-fast versus accumulating
		error-handling. We might expect product applied to Either to accumulate
		errors instead of fail fast. Again, perhaps surprisingly, we find that product
		implements the same fail-fast behaviour as flatMap:
	*/
	
		import cats.instances.either._ // for Semigroupal
		type ErrorOr[A] = Either[Vector[String], A]

		Semigroupal[ErrorOr].product(
			Left(Vector("Error 1")),
			Left(Vector("Error 2"))
		)                                 //> res15: book.ch6.semigroupal.applicative.a_semigroupal.ErrorOr[(Nothing, No
                                                  //| thing)] = Left(Vector(Error 1))

	/*
		In this example product sees the first failure and stops, even though it is possible
		to examine the second parameter and see that it is also a failure.

		6.3.1 Semigroupal Applied to Monads

		The reason for the surprising results for List and Either is that they are
		both monads. To ensure consistent semantics, Cats’ Monad (which extends
		Semigroupal) provides a standard definition of product in terms of map and
		flatMap. This gives what we might think of as unexpected and less useful behaviour
		for a number of data types. The consistency of semantics is important
		for higher level abstractions, but we don’t know about those yet.

		Even our results for Future are a trick of the light. flatMap provides sequen
		tial ordering, so product provides the same. The parallel execution we
		observe occurs because our constituent Futures start running before we call
		product. This is equivalent to the classic create-then-flatMap pattern:
	*/
	
		val a = Future("Future 1")        //> a  : scala.concurrent.Future[String] = Future(<not completed>)
		val b = Future("Future 2")        //> b  : scala.concurrent.Future[String] = Future(<not completed>)

		for {
			x <- a
			y <- b
		} yield (x, y)                    //> res16: scala.concurrent.Future[(String, String)] = Future(<not completed>)
                                                  //| 

	/*
		So why bother with Semigroupal at all? The answer is that we can create
		useful data types that have instances of Semigroupal (and Applicative)
		but not Monad. This frees us to implement product in different ways. We’ll
		examine this further in a moment when we look at an alternative data type for
		error handling.

		6.3.1.1 Exercise: The Product of Monads

		Implement product in terms of flatMap:

		import cats.Monad

		def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
			???
		
		See the solution
		----------------------- solution ------------------------------
		F.1 The Product of Monads
		We can implement product in terms of map and flatMap like so:
	*/

  	import cats.Monad
		import cats.syntax.flatMap._ // for flatMap
		import cats.syntax.functor._ // for map

		def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
			x.flatMap(a => y.map(b => (a, b)))
                                                  //> product: [M[_], A, B](x: M[A], y: M[B])(implicit evidence$1: cats.Monad[M]
                                                  //| )M[(A, B)]
	/*
		Unsurprisingly, this code is equivalent to a for comprehension:
	*/
		def product2[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
			for {
				a <- x
				b <- y
			} yield (a, b)            //> product2: [M[_], A, B](x: M[A], y: M[B])(implicit evidence$2: cats.Monad[M
                                                  //| ])M[(A, B)]
	/*
		The semantics of flatMap are what give rise to the behaviour for List and
		Either:
	*/
	
		import cats.instances.list._ // for Semigroupal
		product2(List(1, 2), List(3, 4))  //> res17: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))

//		type ErrorOr[A] = Either[Vector[String], A]

		product2[ErrorOr, Int, Int](
			Left(Vector("Error 1")),
			Left(Vector("Error 2"))
		)                                 //> res18: book.ch6.semigroupal.applicative.a_semigroupal.ErrorOr[(Int, Int)] 
                                                  //| = Left(Vector(Error 1))

	/*
	  ---------------------------------------------------------------
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
			Validated.invalid(List("Error 2"))
		)                                 //> res19: book.ch6.semigroupal.applicative.a_semigroupal.AllErrorsOr[(Nothing
                                                  //| , Nothing)] = Invalid(List(Error 1, Error 2))
	
	/*
		Validated complements Either nicely. Between the two we have support
		for both of the common types of error handling: fail-fast and accumulating.

		6.4.1 Creating Instances of Validated

		Validated has two subtypes, Validated.Valid and Validated.Invalid,
		that correspond loosely to Right and Left. There are a lot of ways to create
		instances of these types. We can create them directly using their apply
		methods:
	*/
		val v = Validated.Valid(123)      //> v  : cats.data.Validated.Valid[Int] = Valid(123)

		val i = Validated.Invalid(List("Badness"))
                                                  //> i  : cats.data.Validated.Invalid[List[String]] = Invalid(List(Badness))
	
	/*
		However, it is often easier to use the valid and invalid smart constructors,
		which widen the return type to Validated:
	*/
	
		val v1 = Validated.valid[List[String], Int](123)
                                                  //> v1  : cats.data.Validated[List[String],Int] = Valid(123)

		val i1 = Validated.invalid[List[String], Int](List("Badness"))
                                                  //> i1  : cats.data.Validated[List[String],Int] = Invalid(List(Badness))

	/*
		As a third option we can import the valid and invalid extension methods
		from cats.syntax.validated:
	*/
	
		import cats.syntax.validated._ // for valid and invalid

		123.valid[List[String]]           //> res20: cats.data.Validated[List[String],Int] = Valid(123)

		List("Badness").invalid[Int]      //> res21: cats.data.Validated[List[String],Int] = Invalid(List(Badness))

	/*
		As a fourth option we can use pure and raiseError from
		cats.syntax.applicative and cats.syntax.applicativeError
		respectively:
	*/
	
		import cats.syntax.applicative._ // for pure
		import cats.syntax.applicativeError._ // for raiseError

		type ErrorsOr[A] = Validated[List[String], A]

		123.pure[ErrorsOr]                //> res22: book.ch6.semigroupal.applicative.a_semigroupal.ErrorsOr[Int] = Vali
                                                  //| d(123)

		List("Badness").raiseError[ErrorsOr, Int]
                                                  //> res23: book.ch6.semigroupal.applicative.a_semigroupal.ErrorsOr[Int] = Inva
                                                  //| lid(List(Badness))

	/*
		Finally, there are helper methods to create instances of Validated from different
		sources. We can create them from Exceptions, as well as instances of
		Try, Either, and Option:
	*/
	
		Validated.catchOnly[NumberFormatException]("foo".toInt)
                                                  //> res24: cats.data.Validated[NumberFormatException,Int] = Invalid(java.lang.
                                                  //| NumberFormatException: For input string: "foo")

		Validated.catchNonFatal(sys.error("Badness"))
                                                  //> res25: cats.data.Validated[Throwable,Nothing] = Invalid(java.lang.RuntimeE
                                                  //| xception: Badness)

		Validated.fromTry(scala.util.Try("foo".toInt))
                                                  //> res26: cats.data.Validated[Throwable,Int] = Invalid(java.lang.NumberFormat
                                                  //| Exception: For input string: "foo")

		Validated.fromEither[String, Int](Left("Badness"))
                                                  //> res27: cats.data.Validated[String,Int] = Invalid(Badness)

		Validated.fromOption[String, Int](None, "Badness")
                                                  //> res28: cats.data.Validated[String,Int] = Invalid(Badness)

	/*
		6.4.2 Combining Instances of Validated

		We can combine instances of Validated using any of the methods or syntax
		described for Semigroupal above.

		All of these techniques require an instance of Semigroupal to be in scope. As
		with Either, we need to fix the error type to create a type constructor with
		the correct number of parameters for Semigroupal:
	
				type AllErrorsOr[A] = Validated[String, A]
	
		Validated accumulates errors using a Semigroup, so we need one of those
		in scope to summon the Semigroupal. If no Semigroup is visible at the call
		site, we get an annoyingly unhelpful compilation error:

		// Semigroupal[AllErrorsOr]
		// <console>:28: error: could not find implicit value for parameter
		// instance: cats.Semigroupal[AllErrorsOr]
		// Semigroupal[AllErrorsOr]
		//             ^

		Once we import a Semigroup for the error type, everything works as expected:
	*/
	
		import cats.instances.string._ // for Semigroup

		Semigroupal[AllErrorsOr]          //> res29: cats.Semigroupal[book.ch6.semigroupal.applicative.a_semigroupal.All
                                                  //| ErrorsOr] = cats.data.ValidatedInstances$$anon$1@276438c9

	/*
		As long as the compiler has all the implicits in scope to summon a Semigroupal
		of the correct type, we can use apply syntax or any of the other
		Semigroupal methods to accumulate errors as we like:
	*/
		import cats.syntax.apply._ // for tupled

		(
			"Error 1".invalid[Int],
			"Error 2".invalid[Int]
		).tupled                          //> res30: cats.data.Validated[String,(Int, Int)] = Invalid(Error 1Error 2)

	/*
		As you can see, String isn’t an ideal type for accumulating errors. We commonly
		use Lists or Vectors instead:
	*/
		import cats.instances.vector._ // for Semigroupal

		(
			Vector(404).invalid[Int],
			Vector(500).invalid[Int]
		).tupled                          //> res31: cats.data.Validated[scala.collection.immutable.Vector[Int],(Int, In
                                                  //| t)] = Invalid(Vector(404, 500))

	/*
		The cats.data package also provides the NonEmptyList and NonEmptyVector
		types that prevent us failing without at least one error:
	*/
	
		import cats.data.NonEmptyVector

		(
			NonEmptyVector.of("Error 1").invalid[Int],
			NonEmptyVector.of("Error 2").invalid[Int]
		).tupled                          //> res32: cats.data.Validated[cats.data.NonEmptyVector[String],(Int, Int)] = 
                                                  //| Invalid(NonEmptyVector(Error 1, Error 2))

	/*
		6.4.3 Methods of Validated

		Validated comes with a suite of methods that closely resemble those available
		for Either, including the methods from cats.syntax.either. We can
		use map, leftMap, and bimap to transform the values inside the valid and
		invalid sides:
	*/
	
		123.valid.map(_ * 100)            //> res33: cats.data.Validated[Nothing,Int] = Valid(12300)

		"?".invalid.leftMap(_.toString)   //> res34: cats.data.Validated[String,Nothing] = Invalid(?)

		123.valid[String].bimap(_ + "!", _ * 100)
                                                  //> res35: cats.data.Validated[String,Int] = Valid(12300)

		"?".invalid[Int].bimap(_ + "!", _ * 100)
                                                  //> res36: cats.data.Validated[String,Int] = Invalid(?!)

	/*
		We can’t flatMap because Validated isn’t a monad. However, we can
		convert back and forth between Validated and Either using the toEither
		and toValidated methods. Note that toValidated comes from
		[cats.syntax.either]:
	*/
	
		import cats.syntax.either._ // for toValidated
		// import cats.syntax.either._

		"Badness".invalid[Int]            //> res37: cats.data.Validated[String,Int] = Invalid(Badness)

		"Badness".invalid[Int].toEither   //> res38: Either[String,Int] = Left(Badness)

		"Badness".invalid[Int].toEither.toValidated
                                                  //> res39: cats.data.Validated[String,Int] = Invalid(Badness)

	/*
		We can even use the withEither method to temporarily convert to an Either
		and convert back again immediately:
	*/

		41.valid[String].withEither(_.flatMap(n => Right(n + 1)))
                                                  //> res40: cats.data.Validated[String,Int] = Valid(42)

	/*
		There is also a withValidated method in cats.syntax.either.
		As with Either, we can use the ensure method to fail with a specified error
		if a predicate does not hold:
	*/
	
		// 123.valid[String].ensure("Negative!")(_ > 0)

	/*
		Finally, we can call getOrElse or fold to extract values from the Valid and
		Invalid cases:
	*/

		"fail".invalid[Int].getOrElse(0)  //> res41: Int = 0

		"fail".invalid[Int].fold(_ + "!!!", _.toString)
                                                  //> res42: String = fail!!!

	/*
		6.4.4 Exercise: Form Validation

		Let’s get used to Validated by implementing a simple HTML registration
		form. We receive request data from the client in a Map[String, String]
		and we want to parse it to create a User object:
	*/
					case class User(name: String, age: Int)

	/*
		Our goal is to implement code that parses the incoming data enforcing the
		following rules:

			• the name and age must be specified;
			• the name must not be blank;
			• the age must be a valid non-negative integer.

		If all the rules pass our parser we should return a User. If any rules fail we
		should return a List of the error messages.

		To implement this example we’ll need to combine rules in sequence and in
		parallel. We’ll use Either to combine computations in sequence using fail-fast
		semantics, and Validated to combine them in parallel using accumulating
		semantics.

		Let’s start with some sequential combination. We’ll define two methods to
		read the "name" and "age" fields:

			• readName will take a Map[String, String] parameter, extract the
			"name" field, check the relevant validation rules, and return an Either[
			List[String], String].

			• readAge will take a Map[String, String] parameter, extract the
			"age" field, check the relevant validation rules, and return an Either[
			List[String], Int].

		We’ll build these methods up from smaller building blocks. Start by defining a
		method getValue that reads a String from the Map given a field name.

		See the solution

		----------------------- solution ------------------------------
			F.2 Form Validation
			We’ll be using Either and Validated so we’ll start with some imports:
	*/
		import cats.data.Validated
		type FormData = Map[String, String]
		type FailFast[A] = Either[List[String], A]
		type FailSlow[A] = Validated[List[String], A]
		
		/*
			The getValue rule extracts a String from the form data. We’ll be using it in
			sequence with rules for parsing Ints and checking values, so we’ll define it to
			return an Either:
		*/

		def getValue(name: String)(data: FormData): FailFast[String] =
			data.get(name).
				toRight(List(s"$name field not specified"))
                                                  //> getValue: (name: String)(data: book.ch6.semigroupal.applicative.a_semigrou
                                                  //| pal.FormData)book.ch6.semigroupal.applicative.a_semigroupal.FailFast[Strin
                                                  //| g]
		
		/*
		We can create and use an instance of getValue as follows:
		*/
		
		val getName = getValue("name") _  //> getName  : book.ch6.semigroupal.applicative.a_semigroupal.FormData => book
                                                  //| .ch6.semigroupal.applicative.a_semigroupal.FailFast[String] = book.ch6.sem
                                                  //| igroupal.applicative.a_semigroupal$$$Lambda$85/624271064@21a947fe
		
		getName(Map("name" -> "Dade Murphy"))
                                                  //> res43: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[String] = R
                                                  //| ight(Dade Murphy)
		
		/*
			In the event of a missing field, our instance returns an error message containing
			an appropriate field name:
		*/

		getName(Map())                    //> res44: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[String] = L
                                                  //| eft(List(name field not specified))
		
	/*
	  ---------------------------------------------------------------


		Next define a method parseInt that consumes a String and parses it as an
		Int.

		See the solution
		----------------------- solution ------------------------------
		F.3 Form Validation Part 2
		We’ll use Either again here. We use Either.catchOnly to consume the
		NumberFormatException from toInt, and we use leftMap to turn it into
		an error message:
	*/
		
		import cats.syntax.either._ // for catchOnly

		type NumFmtExn = NumberFormatException

		def parseInt2(name: String)(data: String): FailFast[Int] =
			Either.catchOnly[NumFmtExn](data.toInt).
				leftMap(_ => List(s"$name must be an integer"))
                                                  //> parseInt2: (name: String)(data: String)book.ch6.semigroupal.applicative.a_
                                                  //| semigroupal.FailFast[Int]
		
		/*
			Note that our solution accepts an extra parameter to name the field we’re
			parsing. This is useful for creating better error messages, but it’s fine if you
			leave it out in your code.
			
			If we provide valid input, parseInt converts it to an Int:
		*/
		
		parseInt2("age")("11")            //> res45: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[Int] = Righ
                                                  //| t(11)
		
		/*
			If we provide erroneous input, we get a useful error message:
		*/
		parseInt2("age")("foo")           //> res46: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[Int] = Left
                                                  //| (List(age must be an integer))

	/*
	  ---------------------------------------------------------------
		Next implement the validation checks: nonBlank to check Strings, and non-
		Negative to check Ints.

		See the solution
		----------------------- solution ------------------------------
		F.4 Form Validation Part 3
		These definitions use the same pa􀂂erns as above:
	*/

		def nonBlank(name: String)(data: String): FailFast[String] =
			Right(data).
				ensure(List(s"$name cannot be blank"))(_.nonEmpty)
                                                  //> nonBlank: (name: String)(data: String)book.ch6.semigroupal.applicative.a_s
                                                  //| emigroupal.FailFast[String]

		def nonNegative(name: String)(data: Int): FailFast[Int] =
			Right(data).
				ensure(List(s"$name must be non-negative"))(_ >= 0)
                                                  //> nonNegative: (name: String)(data: Int)book.ch6.semigroupal.applicative.a_s
                                                  //| emigroupal.FailFast[Int]

	/*
		Here are some examples of use:
	*/
		nonBlank("name")("Dade Murphy")   //> res47: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[String] = R
                                                  //| ight(Dade Murphy)

		nonBlank("name")("")              //> res48: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[String] = L
                                                  //| eft(List(name cannot be blank))

		nonNegative("age")(11)            //> res49: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[Int] = Righ
                                                  //| t(11)

		nonNegative("age")(-1)            //> res50: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[Int] = Left
                                                  //| (List(age must be non-negative))
	/*
	  ---------------------------------------------------------------

		Now combine getValue, parseInt, nonBlank and nonNegative to create
		readName and readAge:

		See the solution
		----------------------- solution ------------------------------
		F.5 Form Validation Part 4
		We use flatMap to combine the rules sequentially:
	*/

		def readName(data: FormData): FailFast[String] =
			getValue("name")(data).
				flatMap(nonBlank("name"))
                                                  //> readName: (data: book.ch6.semigroupal.applicative.a_semigroupal.FormData)b
                                                  //| ook.ch6.semigroupal.applicative.a_semigroupal.FailFast[String]

		def readAge(data: FormData): FailFast[Int] =
			getValue("age")(data).
				flatMap(nonBlank("age")).
				flatMap(parseInt2("age")).
				flatMap(nonNegative("age"))
                                                  //> readAge: (data: book.ch6.semigroupal.applicative.a_semigroupal.FormData)bo
                                                  //| ok.ch6.semigroupal.applicative.a_semigroupal.FailFast[Int]

	/*
		The rules pick up all the error cases we’ve seen so far:
	*/
		readName(Map("name" -> "Dade Murphy"))
                                                  //> res51: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[String] = R
                                                  //| ight(Dade Murphy)

		readName(Map("name" -> ""))       //> res52: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[String] = L
                                                  //| eft(List(name cannot be blank))

		readName(Map())                   //> res53: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[String] = L
                                                  //| eft(List(name field not specified))

		readAge(Map("age" -> "11"))       //> res54: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[Int] = Righ
                                                  //| t(11)

		readAge(Map("age" -> "-1"))       //> res55: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[Int] = Left
                                                  //| (List(age must be non-negative))

		readAge(Map())                    //> res56: book.ch6.semigroupal.applicative.a_semigroupal.FailFast[Int] = Left
                                                  //| (List(age field not specified))

	/*
	  ---------------------------------------------------------------
		Finally, use a Semigroupal to combine the results of readName and readAge
		to produce a User. Make sure you switch from Either to Validated to accumulate
		errors.

		See the solution
		----------------------- solution ------------------------------
		F.6 Form Validation Part 5

		We can do this by switching from Either to Validated and using apply syntax:
	*/

		import cats.instances.list._ // for Semigroupal
		import cats.syntax.apply._ // for mapN

		def readUser(data: FormData): FailSlow[User] =
			(
				readName(data).toValidated,
				readAge(data).toValidated
			).mapN(User.apply)        //> readUser: (data: book.ch6.semigroupal.applicative.a_semigroupal.FormData)b
                                                  //| ook.ch6.semigroupal.applicative.a_semigroupal.FailSlow[book.ch6.semigroupa
                                                  //| l.applicative.a_semigroupal.User]

		readUser(Map("name" -> "Dave", "age" -> "37"))
                                                  //> res57: book.ch6.semigroupal.applicative.a_semigroupal.FailSlow[book.ch6.se
                                                  //| migroupal.applicative.a_semigroupal.User] = Valid(User(Dave,37))

		readUser(Map("age" -> "-1"))      //> res58: book.ch6.semigroupal.applicative.a_semigroupal.FailSlow[book.ch6.se
                                                  //| migroupal.applicative.a_semigroupal.User] = Invalid(List(name field not sp
                                                  //| ecified, age must be non-negative))

	/*

		The need to switch back and forth between Either and Validated is annoying.
		The choice of whether to use Either or Validated as a default is
		determined by context. In application code, we typically find areas that favour
		accumulating semantics and areas that favour fail-fast semantics. We pick the
		data type that best suits our need and switch to the other as necessary in specific
		situations.
	
	  ---------------------------------------------------------------
		6.5 Apply and Applicative

		Semigroupals aren’t mentioned frequently in the wider functional programming
		literature. They provide a subset of the functionality of a related type
		class called an applicative functor (“applicative” for short).

		Semigroupal and Applicative effectively provide alternative encodings of
		the same notion of joining contexts. Both encodings are introduced in the
		same 2008 paper (http://www.staff.city.ac.uk/~ross/papers/Applicative.html)
		by Conor McBride and Ross Paterson².
		 [²Semigroupal is referred to as “monoidal” in the paper.]

		Cats models applicatives using two type classes. The first, cats.Apply, extends
		Semigroupal and Functor and adds an ap method that applies a parameter
		to a function within a context. The second, cats.Applicative, extends
		Apply, adds the pure method introduced in Chapter 4. Here’s a simplified
		definition in code:

			trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
				def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
		
				def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
					ap(map(fa)(a => (b: B) => (a, b)))(fb)
			}
			
			trait Applicative[F[_]] extends Apply[F] {
				def pure[A](a: A): F[A]
			}
		
		Breaking this down, the ap method applies a parameter fa to a function ff
		within a context F[_]. The product method from Semigroupal is defined in
		terms of ap and map.

		Don’t worry too much about the implementation of product—it’s difficult to
		read and the details aren’t particuarly important. The main point is that there
		is a tight relationship between product, ap, and map that allows any one of
		them to be defined in terms of the other two.

		Applicative also introduces the pure method. This is the same pure we
		saw in Monad. It constructs a new applicative instance from an unwrapped
		value. In this sense, Applicative is related to Apply as Monoid is related to
		Semigroup.

		6.5.1 The Hierarchy of Sequencing Type Classes

		With the introduction of Apply and Applicative, we can zoom out and see
		a whole family of type classes that concern themselves with sequencing computa
		tions in different ways. Figure 6.1 shows the relationship between the
		type classes covered in this book³.
		[³See Rob Norris’ infographic for a the complete picture.]
		(https://github.com/tpolecat/cats-infographic)

	
	  +-------------+    +------------+
    |  Cartesian  |    |  Functor   |
    |             |    |            |
    |   product   |    |    map     |
    +------+------+    +------+-----+
           |                  |
           |  +------------+  |
           +--+   Apply    +--+
              |            |
           +--+    ap      +--+
           |  +------------+  |
           |                  |
    +------+------+    +------+-----+
    | Applicative |    |  FlatMap   |
    |             |    |            |
    |    pure     |    |  flatMap   |
    +------+------+    +------+-----+
           |                  |
           |  +-----------+   |
           +--+           +---+
              |   Monad   |
              |           |
              +-----------+

		Figure 6.1: Monad type class hierarchy
		
		Each type class in the hierarchy represents a particular set of sequencing seman
		tics, introduces a set of characteristic methods, and defines the functionality
		of its supertypes in terms of them:

			• every monad is an applicative;
			• every applicative a semigroupal;
			• and so on.

		Because of the lawful nature of the relationships between the type classes,
		the inheritance relationships are constant across all instances of a type class.
		Apply defines product in terms of ap and map; Monad defines product, ap,
		and map, in terms of pure and flatMap.
		
		To illustrate this let’s consider two hypothetical data types:
		
			• Foo is a monad. It has an instance of the Monad type class that implements
			pure and flatMap and inherits standard definitions of product,
			map, and ap;
	
			• Bar is an applicative functor. It has an instance of Applicative that
			implements pure and ap and inherits standard definitions of product
			and map.

		What can we say about these two data types without knowing more about
		their implementation?

		We know strictly more about Foo than Bar: Monad is a subtype of Applicative,
		so we can guarantee properties of Foo (namely flatMap) that we cannot
		guarantee with Bar. Conversely, we know that Bar may have a wider range
		of behaviours than Foo. It has fewer laws to obey (no flatMap), so it can
		implement behaviours that Foo cannot.

		This demonstrates the classic trade-off of power (in the mathematical sense)
		versus constraint. The more constraints we place on a data type, the more
		guarantees we have about its behaviour, but the fewer behaviours we can
		model.

		Monads happen to be a sweet spot in this trade-off. They are flexible enough
		to model a wide range of behaviours and restrictive enough to give strong guarantees
		about those behaviours. However, there are situations where monads
		aren’t the right tool for the job. Sometimes we want Thai food, and burritos
		just won’t satisfy.

		Whereas monads impose a strict sequencing on the computations they model,
		applicatives and semigroupals impose no such restriction. This puts them in a
		different sweet spot in the hierarchy. We can use them to represent classes
		of parallel / independent computations that monads cannot.

		We choose our semantics by choosing our data structures. If we choose a
		monad, we get strict sequencing. If we choose an applicative, we lose the
		ability to flatMap. This is the trade-off enforced by the consistency laws. So
		choose your types carefully!
		
		6.6 Summary
		While monads and functors are the most widely used sequencing data types
		we’ve covered in this book, semigroupals and applicatives are the most general.
		These type classes provide a generic mechanism to combine values and apply
		functions within a context, from which we can fashion monads and a variety
		of other combinators.

		Semigroupal and Applicative are most commonly used as a means of combining
		independent values such as the results of validation rules. Cats provides
		the Validated type for this specific purpose, along with apply syntax as a convenient
		way to express the combination of rules.

		We have almost covered all of the functional programming concepts on our
		agenda for this book. The next chapter covers Traverse and Foldable, two
		powerful type classes for converting between data types. After that we’ll look
		at several case studies that bring together all of the concepts from Part I.
  */
  
}
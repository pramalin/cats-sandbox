package book.ch6

object a_semigroupal {
  /*
		Chapter 6 Semigroupal and Applicative
		In previous chapters we saw how functors and monads let us sequence opera-
		􀦞ons using map and flatMap. While functors and monads are both immensely
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
      leftMap(_ => s"Couldn't read $str")         //> parseInt: (str: String)Either[String,Int]

  for {
    a <- parseInt("a")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield (a + b + c)                             //> res0: scala.util.Either[String,Int] = Left(Couldn't read a)

  /*
		Another example is the concurrent evaluation of Futures. If we have several

 		long-running independent tasks, it makes sense to execute them concurrently.
		However, monadic comprehension only allows us to run them in sequence.
		map and flatMap aren’t quite capable of capturing what we want because
		they make the assumption that each computation is dependent on the previous
		one:

		// context2 is dependent on value1:
		context1.flatMap(value1 => context2)
	*/

  /*
		6.1 Semigroupal
		cats.Semigroupal is a type class that allows us to combine contexts.

		we have two objects of type F[A] and F[B], a Semigroupal[F] allows us to
		combine them to form an F[(A, B)]. Its definition in Cats is:

		trait Semigroupal[F[_]] {
		def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
	*/

  /*
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

  Semigroupal[Option].product(None, Some("abc"))  //> res2: Option[(Nothing, String)] = None
  Semigroupal[Option].product(Some(123), None)    //> res3: Option[(Int, Nothing)] = None

  /*
		6.1.2 Joining Three or More Contexts
		The companion object for Semigroupal defines a set of methods on top of
		product. For example, the methods tuple2 through tuple22 generalise
		146 CHAPTER 6. SEMIGROUPAL AND APPLICATIVE
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
	*/

  /*
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
  (Option(123), Option("abc")).tupled             //> res8: Option[(Int, String)] = Some((123,abc))

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
		).mapN(Cat.apply)                 //> res10: Option[book.ch6.a_semigroupal.Cat] = Some(Cat(Garfield,1978,Orange &
                                                  //|  black))

  /*
		Internally mapN uses the Semigroupal to extract the values from the Option
		and the Functor to apply the values to the function.
		It’s nice to see that this syntax is type checked. If we supply a function that
		accepts the wrong number or types of parameters, we get a compile error:
	*/

  val add: (Int, Int) => Int = (a, b) => a + b    //> add  : (Int, Int) => Int = book.ch6.a_semigroupal$$$Lambda$28/985397764@57f
                                                  //| ffcd7
  // add: (Int, Int) => Int = <function2>
  // (Option(1), Option(2), Option(3)).mapN(add)
  // <console>:27: error: type mismatch;
  // found : (Int, Int) => Int
  // required: (Int, Int, Int) => ?
  // (Option(1), Option(2), Option(3)).mapN(add)
  // ^

//  (Option("cats"), Option(true)).mapN(add)

  // <console>:27: error: type mismatch;
  // found : (Int, Int) => Int
  // required: (String, Boolean) => ?
  // (Option("cats"), Option(true)).mapN(add)
  // ^

}
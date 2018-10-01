package book.ch10

object a_data_validation {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  /*
		Chapter 10
		Case Study: Data Validation

		In this case study we will build a library for validation. What do we mean by
		validation? Almost all programs must check their input meets certain criteria.
		Usernames must not be blank, email addresses must be valid, and so on. This
		type of validation often occurs in web forms, but it could be performed on
		configuration files, on web service responses, and any other case where we
		have to deal with data that we can’t guarantee is correct. Authentication, for
		example, is just a specialised form of validation.

		We want to build a library that performs these checks. What design goals
		should we have? For inspiration, let’s look at some examples of the types of
		checks we want to perform:

			• A user must be over 18 years old or must have parental consent.
			• A String ID must be parsable as a Int and the Int must correspond
			to a valid record ID.
			• A bid in an auction must apply to one or more items and have a positive
			value.
			• A username must contain at least four characters and all characters
			must be alphanumeric.
			• An email address must contain a single @ sign. Split the string at the @.
			The string to the left must not be empty. The string to the right must
			be at least three characters long and contain a dot.

		With these examples in mind we can state some goals:

			• We should be able to associate meaningful messages with each valida-
			tion failure, so the user knows why their data is not valid.
			• We should be able to combine small checks into larger ones. Taking the
			username example above, we should be able to express this by combining
			a check of length and a check for alphanumeric values.
			• We should be able to transform data while we are checking it. There
			is an example above requiring we parse data, changing its type from
			String to Int.
			• Finally, we should be able to accumulate all the failures in one go, so
			the user can correct all the issues before resubmiting.

		These goals assume we’re checking a single piece of data. We will also need to
		combine checks across multiple pieces of data. For a login form, for example,
		we’ll need to combine the check results for the username and the password.
		This will turn out to be quite a small component of the library, so the majority
		of our time will focus on checking a single data item.

		10.1 Sketching the Library Structure

		Let’s start at the bottom, checking individual pieces of data. Before we start
		coding let’s try to develop a feel for what we’ll be building. We can use a
		graphical notation to help us. We’ll go through our goals one by one.

		Providing error messages

		Our first goal requires us to associate useful error messages with a check failure.
		The output of a check could be either the value being checked, if it passed
							F[A]
			(Figure 10.1: A validation result)
						A => F[A]
			(Figure 10.2: A validation check)

		the check, or some kind of error message. We can abstractly represent this as
		a value in a context, where the context is the possibility of an error message
		as shown in Figure 10.1.

		A check itself is therefore a function that transforms a value into a value in a
		context as shown in Figure 10.2.

		Combine checks
		How do we combine smaller checks into larger ones? Is this an applicative or
		semigroupal as shown in Figure 10.3?

		Not really. With applicative combination, both checks are applied to the same
		value and result in a tuple with the value repeated. What we want feels more
		like a monoid as shown in Figure 10.4. We can define a sensible identity—a
		check that always passes—and two binary combination operators—and and or:
		We’ll probably be using and and or about equally often with our validation

			(A => F[A] , A => F[A]).tupled returns A => F[(A, A)]
			(Figure 10.3: Applicative combination of checks)

			A => F[A] |+| A => F[A] returns A => F[A]
			(Figure 10.4: Monoid combination of checks)

			A => F[B] map B => C returns A => F[C]

			A => F[B] flatMap B => (A => F[C]) returns A => F[C]

			(Figure 10.5: Monadic combination of checks)

		library and it will be annoying to continuously switch between two monoids
		for combining rules. We consequently won’t actually use the monoid API: we’ll
		use two separate methods, and and or, instead.

		Accumulating errors as we check

		Monoids also feel like a good mechanism for accumulating error messages.
		If we store messages as a List or NonEmptyList, we can even use a preexis
		ting monoid from inside Cats.

		Transforming data as we check it

		In addition to checking data, we also have the goal of transforming it. This
		seems like it should be a map or a flatMap depending on whether the transform
		can fail or not, so it seems we also want checks to be a monad as shown
		in Figure 10.5.

		We’ve now broken down our library into familiar abstractions and are in a good
		position to begin development.

Page 203
--------
		10.2 The Check Datatype

		Our design revolves around a Check, which we said was a function from a
		value to a value in a context. As soon as you see this description you should
		think of something like

			type Check[A] = A => Either[String, A]

		Here we’ve represented the error message as a String. This is probably not
		the best representation. We may want to accumulate messages in a List, for
		example, or even use a different representation that allows for internationaliza
		tion or standard error codes.

		We could attempt to build some kind of ErrorMessage type that holds all
		the information we can think of. However, we can’t predict the user’s requirements.
		Instead let’s let the user specify what they want. We can do this by
		adding a second type parameter to Check:

			type Check[E, A] = A => Either[E, A]

		We will probably want to add custom methods to Check so let’s declare it as
		a trait instead of a type alias:

			trait Check[E, A] {
				def apply(value: A): Either[E, A]
				// other methods...
			}

		As we said in Essential Scala, there are two functional programming patterns
		that we should consider when defining a trait:
			• we can make it a typeclass, or;
			• we can make it an algebraic data type (and hence seal it).

		Type classes allow us to unify disparate data types with a common interface.
		This doesn’t seem like what we’re trying to do here. That leaves us with an

			E • E => E
			List[String] • List[String] => List[String]
			(Figure 10.6: Combining error messages)

		algebraic data type. Let’s keep that thought in mind as we explore the design
		a bit further.

		10.3 Basic Combinators

		Let’s add some combinator methods to Check, starting with and. This method
		combines two checks into one, succeeding only if both checks succeed. Think
		about implementing this method now. You should hit some problems. Read
		on when you do!

			trait Check[E, A] {
				def and(that: Check[E, A]): Check[E, A] =
					???
				// other methods...
			}

		The problem is: what do you do when both checks fail? The correct thing to
		do is to return both errors, but we don’t currently have any way to combine
		Es. We need a type class that abstracts over the concept of “accumulating”
		errors as shown in Figure 10.6 What type class do we know that looks like
		this? What method or operator should we use to implement the • operation?
		See the solution
	*/

  /*
			J.1 Basic Combinators
			We need a Semigroup for E. Then we can combine values of E using the
			combine method or its associated |+| syntax:
	*/
  import cats.Semigroup
  import cats.instances.list._ // for Semigroup
  import cats.syntax.semigroup._ // for |+|
  val semigroup = Semigroup[List[String]]         //> semigroup  : cats.kernel.Semigroup[List[String]] = cats.kernel.instances.Li
                                                  //| stMonoid@7f9a81e8

  semigroup.combine(List("Badness"), List("More badness"))
                                                  //> res0: List[String] = List(Badness, More badness)
  // Combination using Semigroup syntax
  List("Oh noes") |+| List("Fail happened")       //> res1: List[String] = List(Oh noes, Fail happened)

  /*
		Note we don’t need a full Monoid because we don’t need the identity element.
		We should always try to keep our constraints as small as possible!
	*/

  /*
page 205
--------
		10.4. TRANSFORMING DATA 205

		There is another semantic issue that will come up quite quickly: should and
		short-circuit if the first check fails. What do you think the most useful behaviour
		is?

		See the solution
	*/

  /*
			J.2 Basic Combinators Part 2

			We want to report all the errors we can, so we should prefer not shortcircui
			ting whenever possible.

			In the case of the and method, the two checks we’re combining are independent
			of one another. We can always run both rules and combine any errors
			we see.
		*/

  /*
		Use this knowledge to implement and. Make sure you end up with the behaviour
		you expect!
		See the solution
	*/

  // Let’s test the behaviour we get. First we’ll setup some checks:

  import sandbox.usecases.Validation1._
  import cats.instances.list._ // for Semigroup
  import cats.syntax.either._ // for asLeft and asRight

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }                                             //> a  : sandbox.usecases.Validation1.CheckF[List[String],Int] = CheckF(book.ch
                                                  //| 10.a_data_validation$$$Lambda$11/1379435698@29774679)

  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }                                             //> b  : sandbox.usecases.Validation1.CheckF[List[String],Int] = CheckF(book.ch
                                                  //| 10.a_data_validation$$$Lambda$12/1073502961@5e5792a0)

  val check: CheckF[List[String], Int] =
    a and b                                       //> check  : sandbox.usecases.Validation1.CheckF[List[String],Int] = CheckF(san
                                                  //| dbox.usecases.Validation1$CheckF$$Lambda$13/1757676444@ae45eb6)

  // Now run the check with some data:
  check(5)                                        //> res2: Either[List[String],Int] = Left(List(Must be < -2))
  check(0)                                        //> res3: Either[List[String],Int] = Left(List(Must be > 2, Must be < -2))

  /*
		Excellent! Everything works as expected! We’re running both checks and accumula
		ting errors as required.

		What happens if we try to create checks that fail with a type that we can’t
		accumulate? For example, there is no Semigroup instance for Nothing. What
		happens if we create instances of CheckF[Nothing, A]?
	*/

  val a1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)                        //> a1  : sandbox.usecases.Validation1.CheckF[Nothing,Int] = CheckF(book.ch10.a
                                                  //| _data_validation$$$Lambda$14/517210187@ff5b51f)
  val b1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)                        //> b1  : sandbox.usecases.Validation1.CheckF[Nothing,Int] = CheckF(book.ch10.a
                                                  //| _data_validation$$$Lambda$15/633070006@5702b3b1)

  /*
		We can create checks just fine but when we come to combine them we get an
		error we we might expect:

		  val check = a1 and b1
		  // <console>:31: error: could not find implicit value for parameter s:
		  // cats.Semigroup[Nothing]
		  // val check = a and b
		  // ^
	*/

  /*
		Now let’s see another implementation strategy. In this approach we model
		checks as an algebraic data type, with an explicit data type for each combinator.
		We’ll call this implementation Check:
	*/

  // Let’s see an example:
  val a2: Check[List[String], Int] =
    Pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }                                             //> a2  : sandbox.usecases.Validation1.Check[List[String],Int] = Pure(book.ch1
                                                  //| 0.a_data_validation$$$Lambda$16/1776957250@4b952a2d)

  val b2: Check[List[String], Int] =
    Pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }                                             //> b2  : sandbox.usecases.Validation1.Check[List[String],Int] = Pure(book.ch1
                                                  //| 0.a_data_validation$$$Lambda$17/827966648@73846619)

  val check2: Check[List[String], Int] =
    a2 and b2                                     //> check2  : sandbox.usecases.Validation1.Check[List[String],Int] = And(Pure(
                                                  //| book.ch10.a_data_validation$$$Lambda$16/1776957250@4b952a2d),Pure(book.ch1
                                                  //| 0.a_data_validation$$$Lambda$17/827966648@73846619))

  check2(9)                                       //> res4: Either[List[String],Int] = Left(List(Must be < -2))
  check2(0)                                       //> res5: Either[List[String],Int] = Left(List(Must be > 2, Must be < -2))

  /*
			While the ADT implementation is more verbose than the function wrapper
			implementation, it has the advantage of cleanly separating the structure of
			the computation (the ADT instance we create) from the process that gives it
			meaning (the apply method). From here we have a number of options:

					• inspect and refactor checks after they are created;
					• move the apply “interpreter” out into its own module;
					• implement alternative interpreters providing other functionality (for example
					visualizing checks).

			Because of its flexibility, we will use the ADT implementa􀦞on for the rest of
			this case study.
	*/

  /*
		Strictly speaking, Either[E, A] is the wrong abstraction for the output of
		our check. Why is this the case? What other data type could we use instead?
		Switch your implementation over to this new data type.

		See the solution
		Validation2
	*/

  /*
		Our implementation is looking pretty good now. Implement an or combinator
		to compliment and.
		See the solution

		Validation3
	*/

  /*
		With and and or we can implement many of checks we’ll want in practice.
		However, we still have a few more methods to add. We’ll turn to map and
		related methods next.

		10.4 Transforming Data

		One of our requirements is the ability to transform data. This allows us to
		support additional scenarios like parsing input. In this section we’ll extend our
		check library with this additional functionality.

		The obvious starting point is map. When we try to implement this, we immediately
		run into a wall. Our current definition of Check requires the input and
		output types to be the same:

				type Check[E, A] = A => Either[E, A]

		When we map over a check, what type do we assign to the result? It can’t be
		A and it can’t be B. We are at an impasse:

				def map(check: Check[E, A])(func: A => B): Check[E, ???]

		To implement map we need to change the definition of Check. Specifically, we
		need to a new type variable to separate the input type from the output:

				type Check[E, A, B] = A => Either[E, B]

		Checks can now represent operations like parsing a String as an Int:

				val parseInt: Check[List[String], String, Int] =
				// etc...

		However, spliting our input and output types raises another issue. Up until
		now we have operated under the assumption that a Check always returns its
		input when successful. We used this in and and or to ignore the output of the
		le􀁛 and right rules and simply return the original input on success:

				(this(a), that(a)) match {
					case And(left, right) =>
						(left(a), right(a))
							.mapN((result1, result2) => Right(a))

				// etc...
				}

		In our new formulation we can’t return Right(a) because its type is Either[
		E, A] not Either[E, B]. We’re forced to make an arbitrary choice
		between returning Right(result1) and Right(result2). The same is true
		of the or method. From this we can derive two things:

			• we should strive to make the laws we adhere to explicit; and
			• the code is telling us we have the wrong abstraction in Check.

		10.4.1 Predicates

		We can make progress by pulling apart the concept of a predicate, which can
		be combined using logical operations such as and and or, and the concept of
		a check, which can transform data.

		What we have called Check so far we will call Predicate. For Predicate
		we can state the following identity law encoding the notion that a predicate
		always returns its input if it succeeds:

				For a predicate p of type Predicate[E, A] and elements a1
				and a2 of type A, if p(a1) == Success(a2) then a1 == a2.

		Making this change gives us the following code:

				(sandbox.usecases.Predicate)

		10.4.2 Checks

		We’ll use Check to represent a structure we build from a Predicate that also
		allows transformation of its input. Implement Check with the following interface:

			sealed trait Check[E, A, B] {
				def apply(a: A): Validated[E, B] =
					???
				def map[C](func: B => C): Check[E, A, C] =
					???
			}

			See the solution

			sandbox.usecases.Validation4
	*/

  /*
		What about flatMap? The semantics are a bit unclear here. The method is
		simple enough to declare but it’s not so obvious what it means or how we
		should implement apply. The general shape of flatMap is shown in Figure
		10.7.

		How do we relate F in the figure to Check in our code? Check has three type
		variables while F only has one.

		To unify the types we need to fix two of the type parameters. The idiomatic
		choices are the error type E and the input type A. This gives us the relationships
		shown in Figure 10.8. In other words, the semantics of applying a FlatMap
		are:

				F[A] flatMap A => F[B] returns F[B]

				(Figure 10.7: Type chart for flatMap)

				A => F[B] flatMap B => (A => F[C]) returns A => F[C]

				(Figure 10.8: Type chart for flatMap applied to Check)

			• given an input of type A, convert to F[B];
			• use the output of type B to choose a Check[E, A, C];
			• return to the original input of type A and apply it to the chosen check
			to generate the final result of type F[C].

		This is quite an odd method. We can implement it, but it is hard to find a use
		for it. Go ahead and implement flatMap for Check, and then we’ll see a more
		generally useful method.

		See the solution

		sandbox.usecases.Validation5
	*/

  /*
		We can write a more useful combinator that chains together two Checks. The
		output of the first check is connected to the input of the second. This is analogous
		to function composition using andThen:

				val f: A => B = ???
				val g: B => C = ???
				val h: A => C = f andThen g

		A Check is basically a function A => Validated[E, B] so we can define an
		analagous andThen method:

page 210
--------

				trait Check[E, A, B] {
					def andThen[C](that: Check[E, B, C]): Check[E, A, C]
				}
		Implement andThen now!
		See the solution

		sandbox.usecases.Validation6
*/

  /*
		10.4.3 Recap

		We now have two algebraic data types, Predicate and Check, and a host
		of combinators with their associated case class implementations. Look at the
		following solution for a complete definition of each ADT.

		See the solution

		sandbox.usecases.Validation7

*/

  /*
		We have a complete implementation of Check and Predicate that do most
		of what we originally set out to do. However, we are not finished yet. You
		have probably recognised structure in Predicate and Check that we can abstract
		over: Predicate has a monoid and Check has a monad. Furthermore,
		in implementing Check you might have felt the implementation doesn’t do
		much—all we do is call through to underlying methods on Predicate and
		Validated.

		There are a lot of ways this library could be cleaned up. However, let’s implement
		some examples to prove to ourselves that our library really does work,
		and then we’ll turn to improving it.

		Implement checks for some of the examples given in the introduction:
				• A username must contain at least four characters and consist entirely
				of alphanumeric characters
				• An email address must contain an @ sign. Split the string at the @. The
				string to the left must not be empty. The string to the right must be at
				least three characters long and contain a dot.

		(You might find the following predicates useful:)

		See the solution
		sandbox.usecases.Validation7Test
*/

  /*
    Finally, here’s a check for a User that depends on checkUsername and
    checkEmail:
  */

	import sandbox.usecases.Validation7Test._
  import cats.data.Validated
  import cats.syntax.apply._     // for mapN

  final case class User(username: String, email: String)

  def createUser(
    username: String,
    email:    String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)
                                                  //> createUser: (username: String, email: String)cats.data.Validated[sandbox.u
                                                  //| secases.Validation7Test.Errors,book.ch10.a_data_validation.User]

  /*
  	We can check our work by creating a couple of example users:
	*/
  createUser("Noel", "noel@underscore.io")        //> res6: cats.data.Validated[sandbox.usecases.Validation7Test.Errors,book.ch1
                                                  //| 0.a_data_validation.User] = Valid(User(Noel,noel@underscore.io))
  createUser("", "dave@underscore@io")            //> res7: cats.data.Validated[sandbox.usecases.Validation7Test.Errors,book.ch1
                                                  //| 0.a_data_validation.User] = Invalid(NonEmptyList(Must be longer than 3 cha
                                                  //| racters, Must contain a single @ character))

}
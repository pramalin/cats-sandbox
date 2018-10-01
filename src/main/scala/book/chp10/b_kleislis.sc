package book.chp10

object b_kleislis {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  /*
		10.5 Kleislis

		We’ll finish off this case study by cleaning up the implementation of Check. A
		justifiable criticism of our approach is that we’ve written a lot of code to do
		very little. A Predicate is essentially a function A => Validated[E, A],
		and a Check is basically a wrapper that lets us compose these functions.

		We can abstract A => Validated[E, A] to A => F[B], which you’ll recognise
		as the type of function you pass to the flatMap method on a monad.
		Imagine we have the following sequence of operations:

				A => F[A]	flatMap A => F[B] flatMap B => F[C]
				(Figure 10.9: Sequencing monadic transforms)

			• We lift some value into a monad (by using pure, for example). This is a
			function with type A => F[A].
			• We then sequence some transformations on the monad using flatMap.

		We can illustrate this as shown in Figure 10.9. We can also write out this
		example using the monad API as follows:

				val aToB: A => F[B] = ???
				val bToC: B => F[C] = ???

				def example[A, C](a: A): F[C] =
					aToB(a).flatMap(bToC)

		Recall that Check is, in the abstract, allowing us to compose functions of type
		A => F[B]. We can write the above in terms of andThen as:

				val aToC = aToB andThen bToC

		The result is a (wrapped) function aToC of type A => F[C] that we can subsequently
		apply to a value of type A.

		We have achieved the same thing as the example method without having to
		reference an argument of type A. The andThen method on Check is analogous
		to function composition, but is composing function A => F[B] instead of A
		=> B.

		The abstract concept of composing functions of type A => F[B] has a name:
		a Kleisli.

		Cats contains a data type cats.data.Kleisli that wraps a function just as
		Check does. Kleisli has all the methods of Check plus some additional
		ones. If Kleisli seems familiar to you, then congratulations. You’ve seen
		through its disguise and recognised it as another concept from earlier in the
		book: Kleisli is just another name for ReaderT.

		Here is a simple example using Kleisli to transform an integer into a list of
		integers through three steps:
*/
  import cats.data.Kleisli
  import cats.instances.list._ // for Monad

  /*
			These steps each transform an input Int into an output of type List[Int]:
		*/

  val step1: Kleisli[List, Int, Int] =
    Kleisli(x => List(x + 1, x - 1))              //> step1  : cats.data.Kleisli[List,Int,Int] = Kleisli(book.chp10.b_kleislis$$$
                                                  //| Lambda$8/557041912@6fadae5d)

  step1.run(20)                                   //> res0: List[Int] = List(21, 19)

  val step2: Kleisli[List, Int, Int] =
    Kleisli(x => List(x, -x))                     //> step2  : cats.data.Kleisli[List,Int,Int] = Kleisli(book.chp10.b_kleislis$$$
                                                  //| Lambda$12/940553268@668bc3d5)

  (step1 andThen step2).run(20)                   //> res1: List[Int] = List(21, -21, 19, -19)

  val step3: Kleisli[List, Int, Int] =
    Kleisli(x => List(x * 2, x / 2))              //> step3  : cats.data.Kleisli[List,Int,Int] = Kleisli(book.chp10.b_kleislis$$$
                                                  //| Lambda$16/795372831@3fee9989)

  /*
		We can combine the steps into a single pipeline that combines the underlying
		Lists using flatMap:
*/
  val pipeline = step1 andThen step2 andThen step3//> pipeline  : cats.data.Kleisli[List,Int,Int] = Kleisli(cats.data.Kleisli$$$L
                                                  //| ambda$14/1121647253@73ad2d6)

  /*
		The result is a function that consumes a single Int and returns eight outputs,
		each produced by a different combination of transformations from step1,
		step2, and step3:
*/
  pipeline.run(20)                                //> res2: List[Int] = List(42, 10, -42, -10, 38, 9, -38, -9)

  /*
		The only notable difference between Kleisli and Check in terms of API is
		that Kleisli renames our apply method to run.

		Let’s replace Check with Kleisli in our validation examples. To do so we
		need to make a few changes to Predicate. We must be able to convert
		a Predicate to a function, as Kleisli only works with functions. Somewhat
		more subtly, when we convert a Predicate to a function, it should
		have type A => Either[E, A] rather than A => Validated[E, A] because
		Kleisli relies on the wrapped function returning a monad.

		Add a method to Predicate called run that returns a function of the correct
		type. Leave the rest of the code in Predicate the same.

		See the solution
	*/

  /*
		J.11 Kleislis

		Here’s an abbreviated definition of run. Like apply, the method must accept
		an implicit Semigroup:
	*/

  /*
			  import cats.Semigroup
			  import cats.data.Validated
			  sealed trait Predicate[E, A] {

			    def run(implicit s: Semigroup[E]): A => Either[E, A] =
			      (a: A) => this(a).toEither

			    def apply(a: A): Validated[E, A] =
			      ??? // etc...
			    // other methods...
			  }
			*/

  /*
		Now rewrite our username and email validation example in terms of Kleisli
		and Predicate. Here are few tips in case you get stuck:

		First, remember that the run method on Predicate takes an implicit parameter.
		If you call aPredicate.run(a) it will try to pass the implicit parameter
		explicitly. If you want to create a function from a Predicate and immediately
		apply that function, use aPredicate.run.apply(a)

		Second, type inference can be tricky in this exercise. We found that the following
		definitions helped us to write code with fewer type declarations.

			  type Result[A] = Either[Errors, A]
			  type Check[A, B] = Kleisli[Result, A, B]
			  // Create a check from a function:
			  def check[A, B](func: A => Result[B]): Check[A, B] =
			    Kleisli(func)
			  // Create a check from a Predicate:
			  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
			    Kleisli[Result, A, A](pred.run)

		See the solution
			sandbox.usecases.Validation8
	*/

  import sandbox.usecases.Validation8Test._
  import cats.syntax.apply._     // for mapN
  import cats.instances.either._ // for Semigroupal
     
  final case class User(username: String, email: String)
  
  def createUser(
    username: String,
    email:    String): Either[Errors, User] = (
    checkUsername.run(username),
    checkEmail.run(email)).mapN(User)             //> createUser: (username: String, email: String)Either[sandbox.usecases.Valida
                                                  //| tion8Test.Errors,book.chp10.b_kleislis.User]
    
  createUser("Noel", "noel@underscore.io")        //> res3: Either[sandbox.usecases.Validation8Test.Errors,book.chp10.b_kleislis.
                                                  //| User] = Right(User(Noel,noel@underscore.io))
  
  createUser("", "dave@underscore@io")            //> res4: Either[sandbox.usecases.Validation8Test.Errors,book.chp10.b_kleislis.
                                                  //| User] = Left(NonEmptyList(Must be longer than 3 characters))

  /*
		We have now written our code entirely in terms of Kleisli and Predicate,
		completely removing Check. This is a good first step to simplifying our library.
		There’s still plenty more to do, but we have a sophisticated building block from
		Cats to work with. We’ll leave further improvements up to the reader.

		10.6 Summary
		This case study has been an exercise in removing rather than building abstrac-
		tions. We started with a fairly complex Check type. Once we realised we
		were conflating two concepts, we separated out Predicate leaving us with
		something that could be implemented with Kleisli.
		
		We made several design choices above that reasonable developers may disagree
		with. Should the method that converts a Predicate to a function really
		be called run instead of, say, toFunction? Should Predicate be a subtype
		of Function to begin with? Many functional programmers prefer to avoid
		subtyping because it plays poorly with implicit resolution and type inference,
		but there could be an argument to use it here. As always the best decisions
		depend on the context in which the library will be used.
	*/
	
}
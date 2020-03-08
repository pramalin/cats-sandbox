// package book.ch5.monad.transformers

// oject a_monad_transformers {
  println("Welcome to the Scala worksheet")       
	/*
		Chapter 5

		Monad Transformers

		Monads are like burritos, which means that once you acquire a taste, you’ll
		find yourself returning to them again and again. This is not without issues. As
		burritos can bloat the waist, monads can bloat the code base through nested
		for-comprehensions.

		Imagine we are interacting with a database. We want to look up a user record.
		The user may or may not be present, so we return an Option[User]. Our
		communication with the database could fail for many reasons (network issues,
		authentication problems, and so on), so this result is wrapped up in an Either,
		giving us a final result of Either[Error, Option[User]].

		To use this value we must nest flatMap calls (or equivalently, forcomprehensions):
	
				def lookupUserName(id: Long): Either[Error, Option[String]] =
					for {
						optUser <- lookupUser(id)
					} yield {
						for { user <- optUser } yield user.name
					}
	
		This quickly becomes very tedious.

		5.1 Exercise: Composing Monads

		A question arises. Given two arbitrary monads, can we combine them in some
		way to make a single monad? That is, do monads compose? We can try to
		write the code but we soon hit problems:

		import cats.Monad
		import cats.syntax.applicative._ // for pure
		import cats.syntax.flatMap._ // for flatMap
		import scala.language.higherKinds

		// Hypothetical example. This won't actually compile:
		def compose[M1[_]: Monad, M2[_]: Monad] = {
			type Composed[A] = M1[M2[A]]

			new Monad[Composed] {
				def pure[A](a: A): Composed[A] =
					a.pure[M2].pure[M1]

				def flatMap[A, B](fa: Composed[A])
					(f: A => Composed[B]): Composed[B] =
				// Problem! How do we write flatMap?
				???
			}
		}

		It is impossible to write a general definition of flatMap without knowing
		something about M1 or M2. However, if we do know something about one
		or other monad, we can typically complete this code. For example, if we fix
		M2 above to be Option, a definition of flatMap comes to light:

				def flatMap[A, B](fa: Composed[A])
						(f: A => Composed[B]): Composed[B] =
					fa.flatMap(_.fold(None.pure[M])(f))

		Notice that the definition above makes use of None—an Option-specific concept
		that doesn’t appear in the general Monad interface. We need this extra
		detail to combine Option with other monads. Similarly, there are things about
		other monads that help us write composed flatMap methods for them. This
		is the idea behind monad transformers: Cats defines transformers for a variety
		of monads, each providing the extra knowledge we need to compose that
		monad with others. Let’s look at some examples.

		5.2 A Transformative Example

		Cats provides transformers for many monads, each named with a T suffix: EitherT
		composes Either with other monads, OptionT composes Option, and
		so on.
		
		Here’s an example that uses OptionT to compose List and Option. We can
		use can OptionT[List, A], aliased to ListOption[A] for convenience, to
		transform a List[Option[A]] into a single monad:
	*/
			import cats.data.OptionT
	
			type ListOption[A] = OptionT[List, A]
	/*
		Note how we build ListOption from the inside out: we pass List, the type
		of the outer monad, as a parameter to OptionT, the transformer for the inner
		monad.

		We can create instances of ListOption using the OptionT constructor, or
		more conveniently using pure:
	*/
		import cats.Monad
		import cats.instances.list._ // for Monad
		import cats.syntax.applicative._ // for pure

		val result1: ListOption[Int] = OptionT(List(Option(10)))
                                                  


		val result2: ListOption[Int] = 32.pure[ListOption]
                                                  


	/*
		The map and flatMap methods combine the corresponding methods of List
		and Option into single operations:
	*/
			result1.flatMap { (x: Int) =>
				result2.map { (y: Int) =>
					x + y
				}
			}                         

	/*
		This is the basis of all monad transformers. The combined map and flatMap
		methods allow us to use both component monads without having to recursively
		unpack and repack values at each stage in the computation. Now let’s
		look at the API in more depth.
		
		Complexity of Imports
		
		The imports in the code samples above hint at how everything bolts
		together.
		
		We import cats.syntax.applicative to get the pure syntax. pure
		requires an implicit parameter of type Applicative[ListOption].
		We haven’t met Applicatives yet, but all Monads are also Applicatives
		so we can ignore that difference for now.
		
		In order to generate our Applicative[ListOption] we need instances
		of Applicative for List and OptionT. OptionT is a Cats data
		type so its instance is provided by its companion object. The instance
		for List comes from cats.instances.list.
		
		Notice we’re not importing cats.syntax.functor or
		cats.syntax.flatMap. This is because OptionT is a concrete
		data type with its own explicit map and flatMap methods. It wouldn’t
		cause problems if we imported the syntax—the compiler would ignore
		it in favour of the explicit methods.
		
		Remember that we’re subjecting ourselves to these shenanigans because
		we’re stubbornly refusing to use the universal Cats import,
		cats.implicits. If we did use that import, all of the instances and
		syntax we needed would be in scope and everything would just work.

		5.3 Monad Transformers in Cats

		Each monad transformer is a data type, defined in cats.data, that allows
		us to wrap stacks of monads to produce new monads. We use the monads
		we’ve built via the Monad type class. The main concepts we have to cover to
		understand monad transformers are:

			• the available transformer classes;
			• how to build stacks of monads using transformers;
			• how to construct instances of a monad stack; and
			• how to pull apart a stack to access the wrapped monads.

		5.3.1 The Monad Transformer Classes

		By convention, in Cats a monad Foo will have a transformer class called FooT.
		In fact, many monads in Cats are defined by combining a monad transformer
		with the Id monad. Concretely, some of the available instances are:

			• cats.data.OptionT for Option;
			• cats.data.EitherT for Either;
			• cats.data.ReaderT for Reader;
			• cats.data.WriterT for Writer;
			• cats.data.StateT for State;
			• cats.data.IdT for the Id monad.

		Kleisli Arrows

		In Section 4.8 we mentioned that the Reader monad was a specialisa-
		tion of a more general concept called a “kleisli arrow”, represented in
		Cats as cats.data.Kleisli.

		We can now reveal that Kleisli and ReaderT are, in fact, the same
		thing! ReaderT is actually a type alias for Kleisli. Hence why we were
		creating Readers last chapter and seeing Kleislis on the console.


		5.3.2 Building Monad Stacks

		All of these monad transformers follow the same convention. The transformer
		itself represents the inner monad in a stack, while the first type parameter
		specifies the outer monad. The remaining type parameters are the types we’ve
		used to form the corresponding monads.

		For example, our ListOption type above is an alias for OptionT[List, A]
		but the result is effectively a List[Option[A]]. In other words, we build
		monad stacks from the inside out:

				type ListOption[A] = OptionT[List, A]

		Many monads and all transformers have at least two type parameters, so we
		often have to define type aliases for intermediate stages.
		
		For example, suppose we want to wrap Either around Option. Option is the
		innermost type so we want to use the OptionT monad transformer. We need
		to use Either as the first type parameter. However, Either itself has two
		type parameters and monads only have one. We need a type alias to convert
		the type constructor to the correct shape:
	*/
	
		// Alias Either to a type constructor with one parameter:
		type ErrorOr[A] = Either[String, A]

		// Build our final monad stack using OptionT:
		type ErrorOrOption[A] = OptionT[ErrorOr, A]
	/*
		ErrorOrOption is a monad, just like ListOption. We can use pure, map, and
		flatMap as usual to create and transform instances:
	*/
		import cats.instances.either._ // for Monad
		val a = 10.pure[ErrorOrOption]    


		val b = 32.pure[ErrorOrOption]    


		val c = a.flatMap(x => b.map(y => x + y))
                                                  


	/*
		Things become even more confusing when we want to stack three or more
		monads.
		
		For example, let’s create a Future of an Either of Option. Once again we
		build this from the inside out with an OptionT of an EitherT of Future. However,
		we can’t define this in one line because EitherT has three type parameters:

					case class EitherT[F[_], E, A](stack: F[Either[E, A]]) {
					// etc...
					}

		The three type parameters are as follows:

			• F[_] is the outer monad in the stack (Either is the inner);
			• E is the error type for the Either;
			• A is the result type for the Either.

		This time we create an alias for EitherT that fixes Future and Error and
		allows A to vary:
	*/
	
		import scala.concurrent.Future
		import cats.data.{EitherT, OptionT}
		type FutureEither[A] = EitherT[Future, String, A]
		type FutureEitherOption[A] = OptionT[FutureEither, A]

	/*
		Our mammoth stack now composes three monads and our map and flatMap
		methods cut through three layers of abstraction:
	*/
	
		import cats.instances.future._ // for Monad
		import scala.concurrent.Await
		import scala.concurrent.ExecutionContext.Implicits.global
		import scala.concurrent.duration._

		val futureEitherOr: FutureEitherOption[Int] =
			for {
				a1 <- 10.pure[FutureEitherOption]
				b1 <- 32.pure[FutureEitherOption]
			} yield a1 + b1           


	/*
		Kind Projector

		If you frequently find yourself defining multiple type aliases when building
		monad stacks, you may want to try the Kind Projector compiler plugin.
		Kind Projector enhances Scala’s type syntax to make it easier to
		define partially applied type constructors. For example:
	*/
			import cats.instances.option._ // for Monad
			// import cats.instances.option._

			123.pure[EitherT[Option, String, ?]]
                                                  

	/*
		Kind Projector can’t simplify all type declarations down to a single line,
		but it can reduce the number of intermediate type definitions needed
		to keep our code readable.

		5.3.3 Constructing and Unpacking Instances

		As we saw above, we can create transformed monad stacks using the relevant
		monad transformer’s apply method or the usual pure syntax:
		[Cats provides an instance of MonadError for EitherT, allowing us to create instances
		using raiseError as well as pure.]
	*/
		
		// Create using apply:
		val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
                                                  


		// Create using pure:
		val errorStack2 = 32.pure[ErrorOrOption]
                                                  


	/*
		Once we’ve finished with a monad transformer stack, we can unpack it using
		its value method. This returns the untransformed stack. We can then manipulate
		the individual monads in the usual way:
	*/

		// Extracting the untransformed monad stack:
		errorStack1.value                 


		// Mapping over the Either in the stack:
		errorStack2.value.map(_.getOrElse(-1))
                                                  
		// res13: scala.util.Either[String,Int] = Right(32)

	/*
		Each call to value unpacks a single monad transformer. We may need more
		than one call to completely unpack a large stack. For example, to Await the
		FutureEitherOption stack above, we need to call value twice:
	*/
	
		futureEitherOr                    


		val intermediate = futureEitherOr.value
                                                  


		val stack = intermediate.value    


		Await.result(stack, 1.second)     

	/*
		5.3.4 Default Instances

		Many monads in Cats are defined using the corresponding transformer and
		the Id monad. This is reassuring as it confirms that the APIs for monads and
		transformers are identical. Reader, Writer, and State are all defined in this
		way:
	
					type Reader[E, A] = ReaderT[Id, E, A] // = Kleisli[Id, E, A]
					type Writer[W, A] = WriterT[Id, W, A]
					type State[S, A] = StateT[Id, S, A]

		In other cases monad transformers are defined separately to their corresponding
		monads. In these cases, the methods of the transformer tend to mirror the
		methods on the monad. For example, OptionT defines getOrElse, and EitherT
		defines fold, bimap, swap, and other useful methods.

		5.3.5 Usage Patterns

		Widespread use of monad transformers is sometimes difficult because they
		fuse monads together in predefined ways. Without careful thought, we can
		end up having to unpack and repack monads in different configurations to
		operate on them in different contexts.

		We can cope with this in multiple ways. One approach involves creating a single
		“super stack” and sticking to it throughout our code base. This works if
		the code simple and largely uniform in nature. For example, in a web applica-
		tion, we could decide that all request handlers are asynchronous and all can
		fail with the same set of HTTP error codes. We could design a custom ADT
		representing the errors and use a fusion Future and Either everywhere in
		our code:

					sealed abstract class HttpError
					final case class NotFound(item: String) extends HttpError
					final case class BadRequest(msg: String) extends HttpError
					// etc...
					type FutureEither[A] = EitherT[Future, HttpError, A]
	
		The “super stack” approach starts to fail in larger, more heterogeneous code
		bases where different stacks make sense in different contexts. Another design
		pattern that makes more sense in these contexts uses monad transformers
		as local “glue code”. We expose untransformed stacks at module boundaries,
		transform them to operate on them locally, and untransform them before passing
		them on. This allows each module of code to make its own decisions about
		which transformers to use:
	*/
	
		import cats.data.Writer
		type Logged[A] = Writer[List[String], A]
		
		// Methods generally return untransformed stacks:
		
		def parseNumber(str: String): Logged[Option[Int]] =
			util.Try(str.toInt).toOption match {
				case Some(num) => Writer(List(s"Read $str"), Some(num))
				case None => Writer(List(s"Failed on $str"), None)
			}                         


		// Consumers use monad transformers locally to simplify composition:
		def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
			import cats.data.OptionT
	
			val result = for {
				a2 <- OptionT(parseNumber(a))
				b2 <- OptionT(parseNumber(b))
				c2 <- OptionT(parseNumber(c))
			} yield a2 + b2 + c2

			result.value
		}                                 


		// This approach doesn't force OptionT on other users' code:
		val result3 = addAll("1", "2", "3")
                                                  


		val result4 = addAll("1", "a", "3")
                                                  

	
	/*
		Unfortunately, there aren’t one-size-fits-all approaches to working with
		monad transformers. The best approach for you may depend on a lot of factors:
		the size and experience of your team, the complexity of your code base,
		and so on. You may need to experiment and gather feedback from colleagues
		to determine whether monad transformers are a good fit.

		5.4 Exercise: Monads: Transform and Roll Out

		The Autobots, well-known robots in disguise, frequently send messages during
		battle requesting the power levels of their team mates. This helps them
		coordinate strategies and launch devastating attacks. The message sending
		method looks like this:

					def getPowerLevel(autobot: String): Response[Int] =
						???
	
		Transmissions take time in Earth’s viscous atmosphere, and messages are occasionally
		lost due to satellite malfunction or sabotage by pesky Decepticons.
		[It is a well known fact that Autobot neural nets are implemented in Scala. Decepticon
		brains are, of course, dynamically typed.]
		
		Responses are therefore represented as a stack of monads:
	
					type Response[A] = Future[Either[String, A]]
					// defined type alias Response
	
		Optimus Prime is getting tired of the nested for comprehensions in his neural
		matrix. Help him by rewriting Response using a monad transformer.

		See the solution
		----------------------- solution ------------------------------
		E.1 Monads: Transform and Roll Out

		This is a relatively simple combination. We want Future on the outside and
		Either on the inside, so we build from the inside out using an EitherT of
		Future:
	*/

			import cats.data.EitherT
			import scala.concurrent.Future

			type Response[A] = EitherT[Future, String, A]

	/*
	  ---------------------------------------------------------------
		Now test the code by implementing getPowerLevel to retrieve data from a
		set of imaginary allies. Here’s the data we’ll use:
	*/
					val powerLevels = Map(
						"Jazz" -> 6,
						"Bumblebee" -> 8,
						"Hot Rod" -> 10
					)         

	/*
		If an Autobot isn’t in the powerLevels map, return an error message reporting
		that they were unreachable. Include the name in the message for good effect.

		See the solution
		----------------------- solution ------------------------------
		E.2 Monads: Transform and Roll Out Part 2
	*/
		import cats.instances.future._ // for Monad
		import cats.syntax.flatMap._ // for flatMap
		import scala.concurrent.ExecutionContext.Implicits.global
		
//		type Response[A] = EitherT[Future, String, A]
			def getPowerLevel(ally: String): Response[Int] = {
				powerLevels.get(ally) match {
					case Some(avg) => EitherT.right(Future(avg))
					case None => EitherT.left(Future(s"$ally unreachable"))
			}
		}                                 

		
	/*
	  ---------------------------------------------------------------
		Two autobots can perform a special move if their combined power level is
		greater than 15. Write a second method, canSpecialMove, that accepts the
		names of two allies and checks whether a special move is possible. If either
		ally is unavailable, fail with an appropriate error message:

					def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
					???
		See the solution
		----------------------- solution ------------------------------
			E.3 Monads: Transform and Roll Out Part 3
			We request the power level from each ally and use map and flatMap to combine
			the results:
	*/
			def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
				for {
					power1 <- getPowerLevel(ally1)
					power2 <- getPowerLevel(ally2)
				} yield (power1 + power2) > 15
                                                  

	
	/*
	  ---------------------------------------------------------------
		Finally, write a method tacticalReport that takes two ally names and prints
		a message saying whether they can perform a special move:

					def tacticalReport(ally1: String, ally2: String): String =
					???

		See the solution
		----------------------- solution ------------------------------
			E.4 Monads: Transform and Roll Out Part 4

			We use the value method to unpack the monad stack and Await and fold
			to unpack the Future and Either:
	*/

		import scala.concurrent.Await
		import scala.concurrent.ExecutionContext.Implicits.global
		import scala.concurrent.duration._

		def tacticalReport(ally1: String, ally2: String): String = {
			val stack = canSpecialMove(ally1, ally2).value

			Await.result(stack, 1.second) match {
				case Left(msg) =>
					s"Comms error: $msg"
				case Right(true) =>
					s"$ally1 and $ally2 are ready to roll out!"
				case Right(false) =>
					s"$ally1 and $ally2 need a recharge."
				}
		}                                 
		
	/*
	  ---------------------------------------------------------------
		You should be able to use report as follows:
	*/
			tacticalReport("Jazz", "Bumblebee")
                                                  

			tacticalReport("Bumblebee", "Hot Rod")
                                                  

			tacticalReport("Jazz", "Ironhide")
                                                  

	/*
		5.5 Summary

		In this chapter we introduced monad transformers, which eliminate the need
		for nested for comprehensions and pattern matching when working with
		“stacks” of nested monads.

		Each monad transformer, such as FutureT, OptionT or EitherT, provides
		the code needed to merge its related monad with other monads. The transformer
		is a data structure that wraps a monad stack, equipping it with map and
		flatMap methods that unpack and repack the whole stack.

		The type signatures of monad transformers are written from the inside
		out, so an EitherT[Option, String, A] is a wrapper for an Option[
		Either[String, A]]. It is often useful to use type aliases when writing
		transformer types for deeply nested monads.

		With this look at monad transformers, we have now covered everything
		we need to know about monads and the sequencing of computations using
		flatMap. In the next chapter we will switch tack and discuss two new type
		classes, Semigroupal and Applicative, that support new kinds of operation
		such as zipping independent values within a context.
		
	*/
// }
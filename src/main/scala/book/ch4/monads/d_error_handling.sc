package book.ch4.monads

object d_error_handling {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
/*
		4.5 Aside: Error Handling and MonadError
		
		Cats provides an additional type class called MonadError that abstracts over
		Either-like data types that are used for error handling. MonadError provides
		extra operations for raising and handling errors.
		
			This Section is Optional!
			You won’t need to use MonadError unless you need to abstract over
			error handling monads. For example, you can use MonadError to abstract
			over Future and Try, or over Either and EitherT (which we
			will meet in Chapter 5).
			
			If you don’t need this kind of abstraction right now, feel free to skip
			onwards to Section 4.6.
		
		4.5.1 The MonadError Type Class

		Here is a simplified version of the definition of MonadError:
	
					package cats

					trait MonadError[F[_], E] extends Monad[F] {
						// Lift an error into the `F` context:
						def raiseError[A](e: E): F[A]

						// Handle an error, potentially recovering from it:
						def handleError[A](fa: F[A])(f: E => A): F[A]

						// Test an instance of `F`,
						// failing if the predicate is not satisfied:
						def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
					}
	

		MonadError is defined in terms of two type parameters:
		• F is the type of the monad;
		• E is the type of error contained within F.
		
		To demonstrate how these parameters fit together, here’s an example where
		we instantiate the type class for Either:
	*/
		import cats.MonadError
		import cats.instances.either._ // for MonadError
		
		type ErrorOr[A] = Either[String, A]
		
		val monadError = MonadError[ErrorOr, String]

	/*
			ApplicativeError
			
			In reality, MonadError extends another type class called ApplicativeError.
			However, we won’t encounter Applicatives until Chapter
			6. The semantics are the same for each type class so we can ignore
			this detail for now.
		
		4.5.2 Raising and Handling Errors

		The two most important methods of MonadError are raiseError and handleError.
		raiseError is like the pure method for Monad except that it creates
		an instance representing a failure:
	*/
	
		val success = monadError.pure(42)

		val failure = monadError.raiseError("Badness")

	/*
		handleError is the compliment of raiseError. It allows us to consume an
		error and (possibly) turn it into a success, similar to the recover method of
		Future:
	*/
		
		monadError.handleError(failure) {
			case "Badness" =>
				monadError.pure("It's ok")
			case other =>
				monadError.raiseError("It's not ok")
		}

	/*
		There is also a third useful method called ensure that implements filterlike
		behaviour. We test the value of a successful monad with a predicate and
		specify an error to raise if the predicate returns false:
	*/
		import cats.syntax.either._ // for asRight

		monadError.ensure(success)("Number too low!")(_ > 1000)

	/*
		Cats provides syntax for raiseError and handleError
		via cats.syntax.applicativeError and ensure via
		cats.syntax.monadError:
	*/

		import cats.syntax.applicative._ // for pure
		import cats.syntax.applicativeError._ // for raiseError etc
		import cats.syntax.monadError._ // for ensure

		val success1 = 42.pure[ErrorOr]

		val failure1 = "Badness".raiseError[ErrorOr, Int]


		success1.ensure("Number to low!")(_ > 1000)

	
	/*
		There are other useful variants of these methods. See the source of
		cats.MonadError and cats.ApplicativeError for more information.

		4.5.3 Instances of MonadError

		Cats provides instances of MonadError for numerous data types including
		Either, Future, and Try. The instance for Either is customisable to any
		error type, whereas the instances for Future and Try always represent errors
		as Throwables:
	*/
	
		import scala.util.Try
		import cats.instances.try_._ // for MonadError

		val exn: Throwable =
			new RuntimeException("It's all gone wrong")
	
		exn.raiseError[Try, Int]

	/*
		4.5.4 Exercise: Abstracting - not found
	*/
}
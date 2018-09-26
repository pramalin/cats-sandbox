package book.ch4

object f_monad_error {
  //
  // 4.5.1 The MonadError Type Class
  //

  /* Here is a simplified version of the definition of MonadError:

	  package cats
		trait MonadError[F[_], E] extends Monad[F] {

			def raiseError[A](e: E): F[A]
			// Handle an error, potentially recovering from it:
			def handleError[A](fa: F[A])(f: E => A): F[A]

			// Test an instance of `F`,
			// failing if the predicate is not satisfied:
			def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
		}
*/
  /*
		MonadError is defined in terms of two type parameters:
		- F is the type of the monad;
		- E is the type of error contained within F.
	*/

  import cats.MonadError
  import cats.instances.either._ // for MonadError

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]    //> monadError  : cats.MonadError[book.ch4.f_monad_error.ErrorOr,String] = cats.
                                                  //| instances.EitherInstances$$anon$1@3b764bce
  //
  // 4.5.2 Raising and Handling Errors
  //

  /*
		The two most important methods of MonadError are raiseError and handleError.
		raiseError is like the pure method for Monad except that it creates
		an instance represen􀦞ng a failure:
	*/
  val success = monadError.pure(42)               //> success  : book.ch4.f_monad_error.ErrorOr[Int] = Right(42)
  val failure = monadError.raiseError("Badness")  //> failure  : book.ch4.f_monad_error.ErrorOr[Nothing] = Left(Badness)

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
  }                                               //> res0: book.ch4.f_monad_error.ErrorOr[book.ch4.f_monad_error.ErrorOr[String]
                                                  //| ] = Right(Right(It's ok))

  monadError.ensure(success)("Number too low!")(_ > 1000)
                                                  //> res1: book.ch4.f_monad_error.ErrorOr[Int] = Left(Number too low!)
  /*
		Cats provides syntax for raiseError and handleError
		via cats.syntax.applicativeError and ensure via
		cats.syntax.monadError:
	*/
  import cats.syntax.applicative._ // for pure
  import cats.syntax.applicativeError._ // for raiseError etc
  import cats.syntax.monadError._ // for ensure

  val success1 = 42.pure[ErrorOr]                 //> success1  : book.ch4.f_monad_error.ErrorOr[Int] = Right(42)
  val failure1 = "Badness".raiseError[ErrorOr, Int]
                                                  //> failure1  : book.ch4.f_monad_error.ErrorOr[Int] = Left(Badness)
  success1.ensure("Number to low!")(_ > 1000)     //> res2: book.ch4.f_monad_error.ErrorOr[Int] = Left(Number to low!)

  //
  // 4.5.3 Instances of MonadError
  //
  /*
		Cats provides instances of MonadError for numerous data types including
		Either, Future, and Try. The instance for Either is customisable to any
		error type, whereas the instances for Future and Try always represent errors
		as Throwables:
	*/

  import scala.util.Try
  import cats.instances.try_._ // for MonadError
  val exn: Throwable =
    new RuntimeException("It's all gone wrong")   //> exn  : Throwable = java.lang.RuntimeException: It's all gone wrong
  exn.raiseError[Try, Int]                        //> res3: scala.util.Try[Int] = Failure(java.lang.RuntimeException: It's all go
                                                  //| ne wrong)

}
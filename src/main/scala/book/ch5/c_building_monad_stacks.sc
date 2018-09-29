package book.ch5

import cats.data.OptionT
import cats.instances.either._ // for Monad
import cats.syntax.applicative._

object c_building_monad_stacks {
  /*
		All of these monad transformers follow the same conven􀦞on. The transformer
		itself represents the inner monad in a stack, while the first type parameter
		specifies the outer monad. The remaining type parameters are the types we’ve
		used to form the corresponding monads.
		For example, our ListOption type above is an alias for OptionT[List, A]
		but the result is effec􀦞vely a List[Option[A]]. In other words, we build
		monad stacks from the inside out:
	*/
  type ListOption[A] = OptionT[List, A]

  /*
		Many monads and all transformers have at least two type parameters, so we
		o􀁛en have to define type aliases for intermediate stages.
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

  val a = 10.pure[ErrorOrOption]                  //> a  : book.ch5.c_building_monad_stacks.ErrorOrOption[Int] = OptionT(Right(So
                                                  //| me(10)))

  val b = 32.pure[ErrorOrOption]                  //> b  : book.ch5.c_building_monad_stacks.ErrorOrOption[Int] = OptionT(Right(So
                                                  //| me(32)))

  val c = a.flatMap(x => b.map(y => x + y))       //> c  : cats.data.OptionT[book.ch5.c_building_monad_stacks.ErrorOr,Int] = Opti
                                                  //| onT(Right(Some(42)))

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
  import cats.data.{ EitherT, OptionT }

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
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b                                 //> futureEitherOr  : book.ch5.c_building_monad_stacks.FutureEitherOption[Int] 
                                                  //| = OptionT(EitherT(Future(<not completed>)))

  Await.result(futureEitherOr.value.value, 10.seconds)
                                                  //> res0: Either[String,Option[Int]] = Right(Some(42))
  /*
		5.3.3 Constructing and Unpacking Instances
		As we saw above, we can create transformed monad stacks using the relevant
		monad transformer’s apply method or the usual pure syntax:
	*/
  // Create using apply:
  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
                                                  //> errorStack1  : cats.data.OptionT[book.ch5.c_building_monad_stacks.ErrorOr,I
                                                  //| nt] = OptionT(Right(Some(10)))

  // Create using pure:
  val errorStack2 = 32.pure[ErrorOrOption]        //> errorStack2  : book.ch5.c_building_monad_stacks.ErrorOrOption[Int] = Option
                                                  //| T(Right(Some(32)))
  /*
		Once we’ve finished with a monad transformer stack, we can unpack it using
		its value method. This returns the untransformed stack. We can then manipulate
		the individual monads in the usual way:
	*/

  // Extracting the untransformed monad stack:
  errorStack1.value                               //> res1: book.ch5.c_building_monad_stacks.ErrorOr[Option[Int]] = Right(Some(10
                                                  //| ))

  // Mapping over the Either in the stack:
  errorStack2.value.map(_.getOrElse(-1))          //> res2: scala.util.Either[String,Int] = Right(32)
  /*
		Each call to value unpacks a single monad transformer. We may need more
		than one call to completely unpack a large stack. For example, to Await the
		FutureEitherOption stack above, we need to call value twice:
		futureEitherOr
	*/
  futureEitherOr                                  //> res3: book.ch5.c_building_monad_stacks.FutureEitherOption[Int] = OptionT(Ei
                                                  //| therT(Future(Success(Right(Some(42))))))
  val intermediate = futureEitherOr.value         //> intermediate  : book.ch5.c_building_monad_stacks.FutureEither[Option[Int]] 
                                                  //| = EitherT(Future(Success(Right(Some(42)))))
  val stack = intermediate.value                  //> stack  : scala.concurrent.Future[Either[String,Option[Int]]] = Future(Succe
                                                  //| ss(Right(Some(42))))
  Await.result(stack, 1.second)                   //> res4: Either[String,Option[Int]] = Right(Some(42))
}
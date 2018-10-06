package book.ch4

object c_either_monad {
/*
		4.4 Either

		Let’s look at another useful monad: the Either type from the Scala standard
		library. In Scala 2.11 and earlier, many people didn’t consider Either a monad
		because it didn’t have map and flatMap methods. In Scala 2.12, however,
		Either became right biased.

		4.4.1 Left and Right Bias

		In Scala 2.11, Either had no default map or flatMap method. This made the
		Scala 2.11 version of Either inconvenient to use in for comprehensions. We
		had to insert calls to .right in every generator clause:
  */

		val either1: Either[String, Int] = Right(10)
                                                  //> either1  : Either[String,Int] = Right(10)
		val either2: Either[String, Int] = Right(32)
                                                  //> either2  : Either[String,Int] = Right(32)
		
		for {
			a <- either1.right
			b <- either2.right
		} yield a + b                     //> res0: scala.util.Either[String,Int] = Right(42)

	/*
		In Scala 2.12, Either was redesigned. The modern Either makes the decision
		that the right side represents the success case and thus supports map and
		flatMap directly. This makes for comprehensions much more pleasant:
	*/

		for {
			a1 <- either1
			b1 <- either2
		} yield a1 + b1                   //> res1: scala.util.Either[String,Int] = Right(42)
		
	/*
		Cats back-ports this behaviour to Scala 2.11 via the cats.syntax.either
		import, allowing us to use right-biased Either in all supported versions of
		Scala. In Scala 2.12+ we can either omit this import or leave it in place without
		breaking anything:
	*/

		for {
			a2 <- either1
			b2 <- either2
		} yield a2 + b2                   //> res2: scala.util.Either[String,Int] = Right(42)

	/*
		4.4.2 Creating Instances

		In addition to creating instances of Left and Right directly, we
		can also import the asLeft and asRight extension methods from
		cats.syntax.either:
	*/
	

		import cats.syntax.either._ // for asRight

		val a3 = 3.asRight[String]        //> a3  : Either[String,Int] = Right(3)

		val b3 = 4.asRight[String]        //> b3  : Either[String,Int] = Right(4)

		for {
			x <- a3
			y <- b3
		} yield x*x + y*y                 //> res3: scala.util.Either[String,Int] = Right(25)

  /*
		These “smart constructors” have advantages over Left.apply and
		Right.apply because they return results of type Either instead of Left
		and Right. This helps avoid type inference bugs caused by over-narrowing,
		like the bug in the example below:


  def countPositive(nums: List[Int]) =
    nums.foldLeft(Right(0)) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }
		// <console>:21: error: type mismatch;
		// found : scala.util.Either[Nothing,Int]
		// required: scala.util.Right[Nothing,Int]
		// accumulator.map(_ + 1)
		//             ^
		// <console>:23: error: type mismatch;
		// found : scala.util.Left[String,Nothing]
		// required: scala.util.Right[Nothing,Int]
		// Left("Negative. Stopping!")
		//       ^


		This code fails to compile for two reasons:

			1. the compiler infers the type of the accumulator as Right instead of
			Either;
			2. we didn’t specify type parameters for Right.apply so the compiler
			infers the left parameter as Nothing.

		Switching to asRight avoids both of these problems. asRight has a return
		type of Either, and allows us to completely specify the type with only one
		type parameter:
	*/

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }                                             //> countPositive: (nums: List[Int])Either[String,Int]

		countPositive(List(1, 2, 3))      //> res4: Either[String,Int] = Right(3)

		countPositive(List(1, -2, 3))     //> res5: Either[String,Int] = Left(Negative. Stopping!)

	/*
		cats.syntax.either adds some useful extension methods to the Either
		companion object. The catchOnly and catchNonFatal methods are great
		for capturing Exceptions as instances of Either:
	*/
	
		Either.catchOnly[NumberFormatException]("foo".toInt)
                                                  //> res6: Either[NumberFormatException,Int] = Left(java.lang.NumberFormatExcept
                                                  //| ion: For input string: "foo")

		Either.catchNonFatal(sys.error("Badness"))
                                                  //> res7: Either[Throwable,Nothing] = Left(java.lang.RuntimeException: Badness)
                                                  //| 

	/*
		There are also methods for creating an Either from other data types:
	*/
		
		Either.fromTry(scala.util.Try("foo".toInt))
                                                  //> res8: Either[Throwable,Int] = Left(java.lang.NumberFormatException: For inp
                                                  //| ut string: "foo")
		Either.fromOption[String, Int](None, "Badness")
                                                  //> res9: Either[String,Int] = Left(Badness)
	/*
		4.4.3 Transforming Eithers
		
		cats.syntax.either also adds some useful methods to instances of Either.
		We can use orElse and getOrElse to extract values from the right side or
		return a default:
	*/

		import cats.syntax.either._

		"Error".asLeft[Int].getOrElse(0)  //> res10: Int = 0

		"Error".asLeft[Int].orElse(2.asRight[String])
                                                  //> res11: Either[String,Int] = Right(2)


	/*
		The ensure method allows us to check whether the right-hand value satisfies
		a predicate:
	*/
	
	val err =	-1.asRight[String].ensure("Must be non-negative!")(_ > 0)
                                                  //> err  : Either[String,Int] = Left(Must be non-negative!)


	/*
		The recover and recoverWith methods provide similar error handling to
		their namesakes on Future:
	*/
		
		"error".asLeft[Int].recover {
			case str: String => -1
		}                                 //> res12: Either[String,Int] = Right(-1)

		"error".asLeft[Int].recoverWith {
			case str: String => Right(-1)
		}                                 //> res13: Either[String,Int] = Right(-1)

	/*
		There are leftMap and bimap methods to complement map:
	*/

		"foo".asLeft[Int].leftMap(_.reverse)
                                                  //> res14: Either[String,Int] = Left(oof)

		6.asRight[String].bimap(_.reverse, _ * 7)
                                                  //> res15: Either[String,Int] = Right(42)

		"bar".asLeft[Int].bimap(_.reverse, _ * 7)
                                                  //> res16: Either[String,Int] = Left(rab)

	/*
		The swap method lets us exchange left for right:
	*/
	
		123.asRight[String]               //> res17: Either[String,Int] = Right(123)

		123.asRight[String].swap          //> res18: scala.util.Either[Int,String] = Left(123)

  /*
		Finally, Cats adds a host of conversion methods: toOption, toList, toTry,
		toValidated, and so on.

		4.4.4 Error Handling

		Either is typically used to implement fail-fast error handling. We sequence
		computations using flatMap as usual. If one computation fails, the remaining
		computations are not run:
	*/

  for {
    a4 <- 1.asRight[String]
    b4 <- 0.asRight[String]
    c4 <- if (b4 == 0) "DIV0".asLeft[Int]
    else (a4 / b4).asRight[String]
  } yield c4 * 100                                //> res19: scala.util.Either[String,Int] = Left(DIV0)

	/*
		When using Either for error handling, we need to determine what type we
		want to use to represent errors. We could use Throwable for this:

					type Result[A] = Either[Throwable, A]

		This gives us similar semantics to scala.util.Try. The problem, however, is
		that Throwable is an extremely broad type. We have (almost) no idea about
		what type of error occurred.

		Another approach is to define an algebraic data type to represent errors that
		may occur in our program:
	*/
	
		sealed trait LoginError extends Product with Serializable
		
		final case class UserNotFound(username: String)
			extends LoginError
		
		final case class PasswordIncorrect(username: String)
			extends LoginError
		
		case object UnexpectedError extends LoginError
		
		case class User(username: String, password: String)
		
		type LoginResult = Either[LoginError, User]

  /*
		This approach solves the problems we saw with Throwable. It gives us a fixed
		set of expected error types and a catch-all for anything else that we didn’t expect.
		We also get the safety of exhaustivity checking on any pattern matching
		we do:
	*/
	  // Choose error-handling behaviour based on type:
	  def handleError(error: LoginError): Unit =
	    error match {
	      case UserNotFound(u) =>
	        println(s"User not found: $u")
	      case PasswordIncorrect(u) =>
	        println(s"Password incorrect: $u")
	      case UnexpectedError =>
	        println(s"Unexpected error")
	    }                                     //> handleError: (error: book.ch4.c_either_monad.LoginError)Unit
	  val result1: LoginResult = User("dave", "passw0rd").asRight
                                                  //> result1  : book.ch4.c_either_monad.LoginResult = Right(User(dave,passw0rd))
                                                  //| 
	
	  val result2: LoginResult = UserNotFound("dave").asLeft
                                                  //> result2  : book.ch4.c_either_monad.LoginResult = Left(UserNotFound(dave))
	
	  result1.fold(handleError, println)      //> User(dave,passw0rd)
	
	  result2.fold(handleError, println)      //> User not found: dave
	

	/*
		4.4.5 Exercise: What is Best?
		Is the error handling strategy in the previous examples well suited for all purposes?
		What other features might we want from error handling?
		See the solution
		
			----------------------- solution ------------------------------
				
				D.3 What is Best?

			This is an open question. It’s also kind of a trick question—the answer depends
			on the semantics we’re looking for. Some points to ponder:

				• Error recovery is important when processing large jobs. We don’t want
				to run a job for a day and then find it failed on the last element.

				• Error reporting is equally important. We need to know what went
				wrong, not just that something went wrong.

				• In a number of cases we want to collect all the errors, not just the first
				one we encountered. A typical example is validating a web form. It’s a
				far better experience to report all errors to the user when they submit
				a form than to report them one at a time.
	
	    ---------------------------------------------------------------
	*/

}
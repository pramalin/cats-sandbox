package book.ch4
import cats.syntax.either._

object d_error_handling {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  // 4.4.4 Error Handling
  for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "DIV0".asLeft[Int]
    else (a / b).asRight[String]
  } yield c * 100                                 //> res0: scala.util.Either[String,Int] = Left(DIV0)

  /*
	When using Either for error handling, we need to determine what type we
	want to use to represent errors. We could use Throwable for this:

	type Result[A] = Either[Throwable, A]

	This gives us similar seman􀦞cs to scala.util.Try. The problem, however, is
	that Throwable is an extremely broad type. We have (almost) no idea about
	what type of error occurred.
	Another approach is to define an algebraic data type to represent errors that
	may occur in our program:
*/
  sealed trait LoginError extends Product with Serializable
  final case class UserNotFound(username: String) extends LoginError
  final case class PasswordIncorrect(username: String) extends LoginError
  case object UnexpectedError extends LoginError
  
  case class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]
  
  /*
	This approach solves the problems we saw with Throwable. It gives us a fixed
	set of expected error types and a catch-all for anything else that we didn’t expect.
	We also get the safety of exhaustivity checking on any pa􀂂ern matching
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
    }                                             //> handleError: (error: book.ch4.d_error_handling.LoginError)Unit
  val result1: LoginResult = User("dave", "passw0rd").asRight
                                                  //> result1  : book.ch4.d_error_handling.LoginResult = Right(User(dave,passw0rd
                                                  //| ))
  val result2: LoginResult = UserNotFound("dave").asLeft
                                                  //> result2  : book.ch4.d_error_handling.LoginResult = Left(UserNotFound(dave))
                                                  //| 
  result1.fold(handleError, println)              //> User(dave,passw0rd)
  result2.fold(handleError, println)              //> User not found: dave
}
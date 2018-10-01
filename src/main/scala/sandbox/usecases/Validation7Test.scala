package sandbox.usecases

object Validation7Test {
  import Validation7._

  /*
  	You might find the following predicates useful:
  */

  import cats.data.NonEmptyList

  type Errors = NonEmptyList[String]
  
  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)
    
  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)
      
  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))
      
  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))
      
  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)

  /*
    J.10 Recap Part 2
    
    Here’s our reference solution. Implementing this required more thought than
    we expected. Switching between Check and Predicate at appropriate places
    felt a bit like guesswork till we got the rule into our heads that Predicate
    304 APPENDIX J. SOLUTIONS FOR: CASE STUDY: DATA VALIDATION
    doesn’t transform its input. With this rule in mind things went fairly smoothly.
    In later sections we’ll make some changes that make the library easier to use.
  */
      
  import cats.syntax.apply._ // for mapN
  import cats.syntax.validated._ // for valid and invalid

  /*
 		Here’s the implementation of checkUsername:
	*/

  // A username must contain at least four characters
  // and consist entirely of alphanumeric characters
  val checkUsername: Check[Errors, String, String] =
    Check(longerThan(3) and alphanumeric)

  /*
		And here’s the implementation of checkEmail, built up from a number of
		smaller components:
	*/

  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.
    
  val splitEmail: Check[Errors, String, (String, String)] =
    Check(_.split('@') match {
      case Array(name, domain) =>
        (name, domain).validNel[String]
    
      case other =>
        "Must contain a single @ character".
          invalidNel[(String, String)]
    })
    
  val checkLeft: Check[Errors, String, String] =
    Check(longerThan(0))
    
  val checkRight: Check[Errors, String, String] =
    Check(longerThan(3) and contains('.'))
    
  val joinEmail: Check[Errors, (String, String), String] =
    Check {
      case (l, r) =>
        (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }
  
  val checkEmail: Check[Errors, String, String] =
    splitEmail andThen joinEmail

    
  /*
    One distinct disadvantage of our example is that it doesn’t tell us where the
    errors came from. We can either achieve that through judicious manipulation
    of error messages, or we can modify our library to track error locations as well
    as messages. Tracking error locations is outside the scope of this case study,
    so we’ll leave this as an exercise to the reader.
  */
}
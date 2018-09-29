package sandbox.semigroupal

object UserFormValidation {
  case class User(name: String, age: Int)
  /*
    Our goal is to implement code that parses the incoming data enforcing the
    following rules:
      • the name and age must be specified;
      • the name must not be blank;
      • the age must be a valid non-negative integer.

    If all the rules pass our parser we should return a User. If any rules fail we
    should return a List of the error messages.

    To implement this example we’ll need to combine rules in sequence and in
    parallel. We’ll use Either to combine computations in sequence using fail-fast
    semantics, and Validated to combine them in parallel using accumulating
    semantics.

    Let’s start with some sequential combination. We’ll define two methods to
    read the "name" and "age" fields:
   */

  /*
   • readName will take a Map[String, String] parameter, extract the
		"name" field, check the relevant validation rules, and return an Either[
		List[String], String].

	 • readAge will take a Map[String, String] parameter, extract the
		"age" field, check the relevant validation rules, and return an Either[
    List[String], Int].

    We’ll build these methods up from smaller building blocks. Start by defining a
    method getValue that reads a String from the Map given a field name.

  	Solution:
  */

  /*
  	F.2 Form Validation
		We’ll be using Either and Validated so we’ll start with some imports:
	*/
  import cats.data.Validated
  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  /*
    The getValue rule extracts a String from the form data. We’ll be using it in
    sequence with rules for parsing Ints and checking values, so we’ll define it to
    return an Either:
  */
  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).
      toRight(List(s"$name field not specified"))

  /*
    Next define a method parseInt that consumes a String and parses it as an
 		Int.
  */

  /*
    F.3 Form Validation Part 2
    We’ll use Either again here. We use Either.catchOnly to consume the
    NumberFormatException from toInt, and we use leftMap to turn it into
    an error message:
  */

  import cats.syntax.either._ // for catchOnly
  type NumFmtExn = NumberFormatException

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumFmtExn](data.toInt).
      leftMap(_ => List(s"$name must be an integer"))
  /*
  	Note that our solution accepts an extra parameter to name the field we’re
  	parsing. This is useful for creating better error messages, but it’s fine if you
  	leave it out in your code.
  */

  /* Next implement the validation checks: nonBlank to check Strings, and non-
		Negative to check Ints.
	*/

  /*
      F.4 Form Validation Part 3
		These defini􀦞ons use the same patterns as above:
	*/
  def nonBlank(name: String)(data: String): FailFast[String] =
    Right(data).
      ensure(List(s"$name cannot be blank"))(_.nonEmpty)
  
  def nonNegative(name: String)(data: Int): FailFast[Int] =
    Right(data).
      ensure(List(s"$name must be non-negative"))(_ >= 0)

  /*
     F.5 Form Validation Part 4
		We use flatMap to combine the rules sequentially:
	*/
        
  def readName(data: FormData): FailFast[String] =
    getValue("name")(data).
      flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data).
      flatMap(nonBlank("age")).
      flatMap(parseInt("age")).
      flatMap(nonNegative("age"))

  /*
    Finally, use a Semigroupal to combine the results of readName and readAge
    to produce a User. Make sure you switch from Either to Validated to accumulate
    errors.
   */

  /*
    F.6 Form Valida􀦞on Part 5
		We can do this by switching from Either to Validated and using apply syntax:
	*/

  import cats.instances.list._ // for Semigroupal
  import cats.syntax.apply._ // for mapN
  
  def readUser(data: FormData): FailSlow[User] =
    
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply)

}


package book.chp10

object test_validation2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	import sandbox.usecases.Validation7Test._
  import cats.data.Validated
  import cats.syntax.apply._     // for mapN

  final case class User(username: String, email: String)

  def createUser(
    username: String,
    email:    String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)
                                                  //> createUser: (username: String, email: String)cats.data.Validated[sandbox.use
                                                  //| cases.Validation7Test.Errors,book.chp10.test_validation2.User]

  /*
  	We can check our work by creating a couple of example users:
	*/
  createUser("Noel", "noel@underscore.io")        //> res0: cats.data.Validated[sandbox.usecases.Validation7Test.Errors,book.chp10
                                                  //| .test_validation2.User] = Valid(User(Noel,noel@underscore.io))
  createUser("", "dave@underscore@io")            //> res1: cats.data.Validated[sandbox.usecases.Validation7Test.Errors,book.chp10
                                                  //| .test_validation2.User] = Invalid(NonEmptyList(Must be longer than 3 charact
                                                  //| ers, Must contain a single @ character))


}
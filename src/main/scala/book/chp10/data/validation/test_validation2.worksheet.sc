// package book.chp10

// oject test_validation2 {
  println("Welcome to the Scala worksheet")       
	import sandbox.usecases.Validation7Test._
  import cats.data.Validated
  import cats.syntax.apply._     // for mapN

  final case class User(username: String, email: String)

  def createUser(
    username: String,
    email:    String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)
                                                  


  /*
  	We can check our work by creating a couple of example users:
	*/
  createUser("Noel", "noel@underscore.io")        

  createUser("", "dave@underscore@io")            




// }
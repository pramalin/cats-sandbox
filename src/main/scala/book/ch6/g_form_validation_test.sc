package book.ch6
import sandbox.semigroupal.UserFormValidation._

object g_form_validation_test {
  // We can create and use an instance of getValue as follows:
  val getName = getValue("name") _                //> getName  : sandbox.semigroupal.UserFormValidation.FormData => sandbox.semigr
                                                  //| oupal.UserFormValidation.FailFast[String] = book.ch6.g_form_validation_test$
                                                  //| $$Lambda$3/1109371569@bebdb06

  getName(Map("name" -> "Dade Murphy"))           //> res0: sandbox.semigroupal.UserFormValidation.FailFast[String] = Right(Dade M
                                                  //| urphy)
  // In the event of a missing field, our instance returns an error message containing
	// an appropriate field name:

	getName(Map())                            //> res1: sandbox.semigroupal.UserFormValidation.FailFast[String] = Left(List(na
                                                  //| me field not specified))

  // If we provide valid input, parseInt converts it to an Int:
	parseInt("age")("11")                     //> res2: sandbox.semigroupal.UserFormValidation.FailFast[Int] = Right(11)

  // If we provide erroneous input, we get a useful error message:
	parseInt("age")("foo")                    //> res3: sandbox.semigroupal.UserFormValidation.FailFast[Int] = Left(List(age m
                                                  //| ust be an integer))

	// Here are some examples of use nonBlank and nonNegative:
	nonBlank("name")("Dade Murphy")           //> res4: sandbox.semigroupal.UserFormValidation.FailFast[String] = Right(Dade M
                                                  //| urphy)
	nonBlank("name")("")                      //> res5: sandbox.semigroupal.UserFormValidation.FailFast[String] = Left(List(na
                                                  //| me cannot be blank))
	nonNegative("age")(11)                    //> res6: sandbox.semigroupal.UserFormValidation.FailFast[Int] = Right(11)
	nonNegative("age")(-1)                    //> res7: sandbox.semigroupal.UserFormValidation.FailFast[Int] = Left(List(age m
                                                  //| ust be non-negative))
	// The rules pick up all the error cases we’ve seen so far:
	readName(Map("name" -> "Dade Murphy"))    //> res8: sandbox.semigroupal.UserFormValidation.FailFast[String] = Right(Dade M
                                                  //| urphy)
	readName(Map("name" -> ""))               //> res9: sandbox.semigroupal.UserFormValidation.FailFast[String] = Left(List(na
                                                  //| me cannot be blank))
	readName(Map())                           //> res10: sandbox.semigroupal.UserFormValidation.FailFast[String] = Left(List(n
                                                  //| ame field not specified))
	readAge(Map("age" -> "11"))               //> res11: sandbox.semigroupal.UserFormValidation.FailFast[Int] = Right(11)
	readAge(Map("age" -> "-1"))               //> res12: sandbox.semigroupal.UserFormValidation.FailFast[Int] = Left(List(age 
                                                  //| must be non-negative))
	readAge(Map())                            //> res13: sandbox.semigroupal.UserFormValidation.FailFast[Int] = Left(List(age 
                                                  //| field not specified))


	readUser(Map("name" -> "Dave", "age" -> "37"))
                                                  //> res14: sandbox.semigroupal.UserFormValidation.FailSlow[sandbox.semigroupal.U
                                                  //| serFormValidation.User] = Valid(User(Dave,37))
	readUser(Map("age" -> "-1"))              //> res15: sandbox.semigroupal.UserFormValidation.FailSlow[sandbox.semigroupal.
                                                  //| UserFormValidation.User] = Invalid(List(name field not specified, age must 
                                                  //| be non-negative))
 
 	/*
		The need to switch back and forth between Either and Validated is annoying.
		The choice of whether to use Either or Validated as a default is
		determined by context. In applica􀦞on code, we typically find areas that favour
		accumula􀦞ng seman􀦞cs and areas that favour fail-fast seman􀦞cs. We pick the
		data type that best suits our need and switch to the other as necessary in specific
		situa􀦞ons.
	*/

}
// package book.chp10

// oject test_validation {
  println("Welcome to the Scala worksheet")       

  import sandbox.usecases.Validation1._
  import cats.syntax.either._ // for asLeft and asRight

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }                                             


	/*
		What happens if we try to create checks that fail with a type that we can’t
		accumulate? For example, there is no Semigroup instance for Nothing. What
		happens if we create instances of CheckF[Nothing, A]?
	*/
	
  val a1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)                        

  val b1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)                        


	// val check = a1 and b1
  /*
		We can create checks just fine but when we come to combine them we get an
		error we we might expect:
		
		Multiple markers at this line
		- could not find implicit value for parameter s: cats.Semigroup[Nothing]
		- not enough arguments for method and: (implicit s: cats.Semigroup[Nothing])sandbox.usecases.Validation.CheckF[Nothing,Int].  Unspecified value parameter s.
	*/


  /*
		Now let’s see another implementation strategy. In this approach we model
		checks as an algebraic data type, with an explicit data type for each combinator.
		We’ll call this implementation Check:
	*/

  import cats.instances.list._ // for Semigroup
  // Let’s see an example:
  val a2: Check[List[String], Int] =
    Pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }                                             

    
  val b2: Check[List[String], Int] =
    Pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }                                             

    
  val check: Check[List[String], Int] =
    a2 and b2                                     



	check(9)                                  
	check(0)                                  
// }
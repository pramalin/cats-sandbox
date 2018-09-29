package book.ch3

	/*
		7.6.1 Take Home Points
		Implicit classes are a Scala language feature that allows us to define extra func􀦞onality on existing data types
		without using conventional inheritance. This is a programming pattern called type enrichment.
		The Scala compiler uses implicit classes to fix type errors in our code. When it encounters us accessing a method
		or field that doesn’t exist, it looks through the available implicits to find some code it can insert to fix the error.
		The rules for implicit classes are the same as for implicit values, with the additional restriction that only a single
		implicit class will be used to fix a type error.

		7.6.2 Exercises
		7.6.2.1 Drinking the Kool Aid
		Use your newfound powers to add a method yeah to Int, which prints Oh yeah! as many times as the Int
		on which it is called if the Int is positive, and is silent otherwise. Here’s an example of usage:
	*/
	
object essential_scala {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  import IntImplicits._

  3.yeah                                          //> Oh yeah!
                                                  //| Oh yeah!
                                                  //| Oh yeah!
 
}
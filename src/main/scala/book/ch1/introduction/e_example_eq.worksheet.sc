// package book.ch1.introduction

// oject e_example_eq {
	/*

		1.5 Example: Eq
	
		We will finish off this chapter by looking at another useful type class: cats.Eq.
		Eq is designed to support type-safe equality and address annoyances using
		Scala’s built-in == operator.
	
		Almost every Scala developer has written code like this before:
	*/
	
		List(1, 2, 3).map(Option(_)).filter(item => item == 1)
                                                  

	/*
		Ok, many of you won’t have made such a simple mistake as this, but the principle
		is sound. The predicate in the filter clause always returns false because
		it is comparing an Int to an Option[Int].

		This is programmer error—we should have compared item to Some(1) instead
		of 1. However, it’s not technically a type error because == works for any pair
		of objects, no matter what types we compare. Eq is designed to add some
		type safety to equality checks and work around this problem.

		1.5.1 Equality, Liberty, and Fraternity

		We can use Eq to define type-safe equality between instances of any given
		type:

				// package cats
				
				trait Eq[A] {
					def eqv(a: A, b: A): Boolean
					// other concrete methods based on eqv...
				}

		The interface syntax, defined in cats.syntax.eq, provides two methods for
		performing equality checks provided there is an instance Eq[A] in scope:

			• === compares two objects for equality;
			• =!= compares two objects for inequality.

		1.5.2 Comparing Ints
		
		Let’s look at a few examples. First we import the type class:
	*/
		import cats.Eq
	/*
		Now let’s grab an instance for Int:
	*/
		import cats.instances.int._ // for Eq
		val eqInt = Eq[Int]               
	/*
		We can use eqInt directly to test for equality:
	*/
		
		eqInt.eqv(123, 123)               
		eqInt.eqv(123, 234)               
	/*
		Unlike Scala’s == method, if we try to compare objects of different types using
		eqv we get a compile error:
	*/
		
//		eqInt.eqv(123, "234")
		// <console>:18: error: type mismatch;
		// found : String("234")
		// required: Int
		// eqInt.eqv(123, "234")
		// ^
	/*
		We can also import the interface syntax in cats.syntax.eq to use the ===
		and =!= methods:
	*/
		
		import cats.syntax.eq._ // for === and =!=
		
		123 === 123                       

		123 =!= 234                       

	/*
		Again, comparing values of different types causes a compiler error:
	*/
		
//		123 === "123"
		// <console>:20: error: type mismatch;
		// found : String("123")
		// required: Int
		// 123 === "123"
		// ^
	/*
		1.5.3 Comparing Options
		
		Now for a more interesting example—Option[Int]. To compare values of
		type Option[Int] we need to import instances of Eq for Option as well as
		Int:
	*/
		import cats.instances.int._ // for Eq
		import cats.instances.option._ // for Eq

	/*
		Now we can try some comparisons:
	*/
		
//		Some(1) === None
		// <console>:26: error: value === is not a member of Some[Int]
		// Some(1) === None
		// ^

	/*
		We have received an error here because the types don’t quite match up. We
		have Eq instances in scope for Int and Option[Int] but the values we are
		comparing are of type Some[Int]. To fix the issue we have to re-type the
		arguments as Option[Int]:
	*/
		(Some(1) : Option[Int]) === (None : Option[Int])
                                                  

	/*
		We can do this in a friendlier fashion using the Option.apply and Option.
		empty methods from the standard library:
*/
		Option(1) === Option.empty[Int]   

	/*
		or using special syntax from cats.syntax.option:
	*/
		
		import cats.syntax.option._ // for some and none

		1.some === none[Int]              

		1.some =!= none[Int]              

	/*
		1.5.4 Comparing Custom Types
	
		We can define our own instances of Eq using the Eq.instance method, which
		accepts a function of type (A, A) => Boolean and returns an Eq[A]:
	*/
		import java.util.Date
		import cats.instances.long._ // for Eq

		implicit val dateEq: Eq[Date] =
			Eq.instance[Date] { (date1, date2) =>
				date1.getTime === date2.getTime
			}                         
			
		val x = new Date() // now         
		val y = new Date() // a bit later than now
                                                  
		
		x === x                           

		x === y                           

	/*
		1.5.5 Exercise: Equality, Liberty, and Felinity
		
		Implement an instance of Eq for our running Cat example:
		
			final case class Cat(name: String, age: Int, color: String)
			
		Use this to compare the following pairs of objects for equality and inequality:
		
			val cat1 = Cat("Garfield", 38, "orange and black")
			val cat2 = Cat("Heathcliff", 33, "orange and black")
			val optionCat1 = Option(cat1)
			val optionCat2 = Option.empty[Cat]

		See the solution
			sandbox.typeclasses.Cat
			sandbox.typeclasses.EqInstances
			sandbox.typec;asses.CatEqApp
	  ----------------------- solution ------------------------------
	*/

		import cats.Eq
		import cats.syntax.eq._ // for ===
	/*
		Our Cat class is the same as ever:
	*/

		final case class Cat(name: String, age: Int, color: String)

	/*
		We bring the Eq instances for Int and String into scope for the implementa-
		tion of Eq[Cat]:
	*/
	
		import cats.instances.int._ // for Eq
		import cats.instances.string._ // for Eq
		
		implicit val catEqual: Eq[Cat] =
			Eq.instance[Cat] { (cat1, cat2) =>
				(cat1.name === cat2.name ) &&
				(cat1.age === cat2.age ) &&
				(cat1.color === cat2.color)
			}                         


	/*
		Finally, we test things out in a sample applica􀦞on:
	*/

		val cat1 = Cat("Garfield", 38, "orange and black")
                                                  


		val cat2 = Cat("Heathcliff", 32, "orange and black")
                                                  

 
 		cat1 === cat2                     

		cat1 =!= cat2                     

		import cats.instances.option._ // for Eq

		val optionCat1 = Option(cat1)     


		val optionCat2 = Option.empty[Cat]

		optionCat1 === optionCat2         

		optionCat1 =!= optionCat2         


	/*
	    ---------------------------------------------------------------
	
		1.6 Controlling Instance Selection

		When working with type classes we must consider two issues that control
		instance selection:

			• What is the relationship between an instance defined on a type and its
			subtypes?
	
			For example, if we define a JsonWriter[Option[Int]], will the expression
			Json.toJson(Some(1)) select this instance? (Remember
			that Some is a subtype of Option).
	
			• How do we choose between type class instances when there are many
			available?
	
			What if we define two JsonWriters for Person? When we write
			Json.toJson(aPerson), which instance is selected?
	
		1.6.1 Variance

		When we define type classes we can add variance annotations to the type
		parameter to affect the variance of the type class and the compiler’s ability to
		select instances during implicit resolution.

		To recap Essential Scala, variance relates to subtypes. We say that B is a subtype
		of A if we can use a value of type B anywhere we expect a value of type
		A.
		
		Co- and contravariance annotations arise when working with type constructors.
		For example, we denote covariance with a + symbol:
		
				trait F[+A] // the "+" means "covariant"
		
		Covariance
		
		Covariance means that the type F[B] is a subtype of the type F[A] if B is a
		subtype of A. This is useful for modelling many types, including collections like
		List and Option:
		
				trait List[+A]
				trait Option[+A]
		
		The covariance of Scala collections allows us to substitute collections of one
		type for another in our code. For example, we can use a List[Circle] anywhere
		we expect a List[Shape] because Circle is a subtype of Shape:
		
				sealed trait Shape
				case class Circle(radius: Double) extends Shape
		
				val circles: List[Circle] = ???
				val shapes: List[Shape] = circles
		
		What about contravariance? We write contravariant type constructors with a
		- symbol like this:
		
			trait F[-A]

		Contravariance

		Confusingly, contravariance means that the type F[B] is a subtype of F[A] if
		A is a subtype of B. This is useful for modelling types that represent processes,
		like our JsonWriter type class above:

				trait JsonWriter[-A] {
					def write(value: A): Json
				}
				// defined trait JsonWriter

		Let’s unpack this a bit further. Remember that variance is all about the ability
		to substitute one value for another. Consider a scenario where we have two
		values, one of type Shape and one of type Circle, and two JsonWriters,
		one for Shape and one for Circle:
		
				val shape: Shape = ???
				val circle: Circle = ???
		
				val shapeWriter: JsonWriter[Shape] = ???
				val circleWriter: JsonWriter[Circle] = ???
		
				def format[A](value: A, writer: JsonWriter[A]): Json =
					writer.write(value)
	
		Now ask yourself the question: “Which of combinations of value and writer
		can I pass to format?” We can combine circle with either writer because
		all Circles are Shapes. Conversely, we can’t combine shape with circleWriter
		because not all Shapes are Circles.
	
		This relationship is what we formally model using contravariance. Json-
		Writer[Shape] is a subtype of JsonWriter[Circle] because Circle is a
		subtype of Shape. This means we can use shapeWriter anywhere we expect
		to see a JsonWriter[Circle].
	
		Invariance
	
		Invariance is actually the easiest situation to describe. It’s what we get when
		we don’t write a + or - in a type constructor:

				trait F[A]

		This means the types F[A] and F[B] are never subtypes of one another, no
		matter what the relationship between A and B. This is the default semantics
		for Scala type constructors.
		
		When the compiler searches for an implicit it looks for one matching the type
		or subtype. Thus we can use variance annotations to control type class instance
		selection to some extent.
		
		There are two issues that tend to arise. Let’s imagine we have an algebraic
		data type like:

				sealed trait A
				final case object B extends A
				final case object C extends A

		The issues are:

			1. Will an instance defined on a supertype be selected if one is available?
			For example, can we define an instance for A and have it work for values
			of type B and C?
			
			2. Will an instance for a subtype be selected in preference to that of a
			supertype. For instance, if we define an instance for A and B, and we
			have a value of type B, will the instance for B be selected in preference
			to A?
		
		It turns out we can’t have both at once. The three choices give us behaviour
		as follows:

		Type Class Variance Invariant Covariant Contravariant
		Supertype instance used? No No Yes
		More specific type preferred? No Yes No

		It’s clear there is no perfect system. Cats generally prefers to use invariant type
		classes. This allows us to specify more specific instances for subtypes if we
		want. It does mean that if we have, for example, a value of type Some[Int],
		our type class instance for Option will not be used. We can solve this problem
		with a type annotation like Some(1) : Option[Int] or by using “smart constructors”
		like the Option.apply, Option.empty, some, and none methods
		we saw in Section 1.5.3.
		
		1.7 Summary
		
		In this chapter we took a first look at type classes. We implemented our own
		Printable type class using plain Scala before looking at two examples from
		Cats—Show and Eq.
		
		We have now seen the general patterns in Cats type classes:

			• The type classes themselves are generic traits in the cats // package.
	
			• Each type class has a companion object with, an apply method for materializing
			instances, one or more construc􀢼on methods for creating instances,
			and a collection of other relevant helper methods.
	
			• Default instances are provided via objects in the cats.instances
			// package, and are organized by parameter type rather than by type class.
	
			• Many type classes have syntax provided via the cats.syntax // package.
	
		In the remaining chapters of Part I we will look at several broad and powerful
		type classes—Semigroup, Monoid, Functor, Monad, Semigroupal, Applicative,
		Traverse, and more. In each case we will learn what functionality
		the type class provides, the formal rules it follows, and how it is implemented
		in Cats. Many of these type classes are more abstract than Show or
		Eq. While this makes them harder to learn, it makes them far more useful for
		solving general problems in our code.
	
	*/

// }
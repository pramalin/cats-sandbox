package book.ch4.monads

object b_id_monad {
	/*
		4.3 The Identity Monad
	
		In the previous section we demonstrated Cats’ flatMap and map syntax by
		writing a method that abstracted over different monads:
  */
  	
		import scala.language.higherKinds
		import cats.Monad
		import cats.syntax.functor._ // for map
		import cats.syntax.flatMap._ // for flatMap

		def sumSquare3[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
			for {
				x <- a
				y <- b
			} yield x*x + y*y         //> sumSquare3: [F[_]](a: F[Int], b: F[Int])(implicit evidence$2: cats.Monad[F])
                                                  //| F[Int]

	/*
		This method works well on Options and Lists but we can’t call it passing in
		plain values:
	*/

		// sumSquare(3, 4)
		// <console>:22: error: no type parameters for method sumSquare: (a: F
		// [Int], b: F[Int])(implicit evidence$1: cats.Monad[F])F[Int] exist
		// so that it can be applied to arguments (Int, Int)
		// --- because ---
		// argument expression's type is not compatible with formal parameter type;
		// found : Int
		// required: ?F[Int]
		// sumSquare(3, 4)
		// ^
		// <console>:22: error: type mismatch;
		// found : Int(3)
		// required: F[Int]
		// sumSquare(3, 4)
		// ^
		// <console>:22: error: type mismatch;
		// found : Int(4)
		// required: F[Int]
		// sumSquare(3, 4)
		// ^
  /*
		It would be incredibly useful if we could use sumSquare with parameters that
		were either in a monad or not in a monad at all. This would allow us to abstract
		over monadic and non-monadic code. Fortunately, Cats provides the Id type
		to bridge the gap:
	*/

		import cats.Id

		sumSquare3(3 : Id[Int], 4 : Id[Int])
                                                  //> res0: cats.Id[Int] = 25

  /*
		Id allows us to call our monadic method using plain values. However, the
		exact semantics are difficult to understand. We cast the parameters to sum-
		Square as Id[Int] and received an Id[Int] back as a result!

		What’s going on? Here is the definition of Id to explain:

					package cats
					
					type Id[A] = A

		Id is actually a type alias that turns an atomic type into a single-parameter
		type constructor. We can cast any value of any type to a corresponding Id:
	*/
	
		"Dave" : Id[String]               //> res1: cats.Id[String] = Dave

		123 : Id[Int]                     //> res2: cats.Id[Int] = 123

		List(1, 2, 3) : Id[List[Int]]

	/*
		Cats provides instances of various type classes for Id, including Functor and
		Monad. These let us call map, flatMap, and pure passing in plain values:
  */
  
		val a = Monad[Id].pure(3)

		val b = Monad[Id].flatMap(a)(_ + 1)

		import cats.syntax.functor._ // for map
		import cats.syntax.flatMap._ // for flatMap

		for {
			x <- a
			y <- b
		} yield x + y

	/*
		The ability to abstract over monadic and non-monadic code is extremely powerful.
		For example, we can run code asynchronously in production using Future
		and synchronously in test using Id. We’ll see this in our first case study
		in Chapter 8.

		4.3.1 Exercise: Monadic Secret Identities
		Implement pure, map, and flatMap for Id! What interesting discoveries do
		you uncover about the implementation?

		See the solution
			----------------------- solution ------------------------------

		D.2 Monadic Secret Identities

		Let’s start by defining the method signatures:
		
			import cats.Id
			
			def pure[A](value: A): Id[A] =
				???
			
			def map[A, B](initial: Id[A])(func: A => B): Id[B] =
				???
			
			def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
				???
		
	
		Now let’s look at each method in turn. The pure operation creates an Id[A]
		from an A. But A and Id[A] are the same type! All we have to do is return the
		initial value:
	*/
		def pure[A](value: A): Id[A] =
			value

		pure(123)

		/*
			The map method takes a parameter of type Id[A], applies a function of type
			A => B, and returns an Id[B]. But Id[A] is simply A and Id[B] is simply B!
			All we have to do is call the function—no packing or unpacking required:
		*/

		def map[A, B](initial: Id[A])(func: A => B): Id[B] =
			func(initial)

		map(123)(_ * 2)

		/*
			The final punch line is that, once we strip away the Id type constructors,
			flatMap and map are actually identical:
		*/

		def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] =
			func(initial)
	
		flatMap(123)(_ * 2)

	/*
		
		This ties in with our understanding of functors and monads as sequencing type
		classes. Each type class allows us to sequence operations ignoring some kind
		of complication. In the case of Id there is no complication, making map and
		flatMap the same thing.
		
		Notice that we haven’t had to write type annotations in the method bodies
		above. The compiler is able to interpret values of type A as Id[A] and vice
		versa by the context in which they are used.
		
		The only restriction we’ve seen to this is that Scala cannot unify types and
		type constructors when searching for implicits. Hence our need to re-type
		Int as Id[Int] in the call to sumSquare at the opening of this section:
			
				sumSquare(3 : Id[Int], 4 : Id[Int])
			
	    ---------------------------------------------------------------
*/
}
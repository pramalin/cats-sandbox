// package book.ch3.functors

// oject a_functors {

  println("Welcome to the Scala worksheet")       

	/*
		Chapter 3
		
		Functors
		
		In this chapter we will investigate functors, an abstraction that allows us to
		represent sequences of operations within a context such as a List, an Option,
		or any one of a thousand other possibilities. Functors on their own aren’t so
		useful, but special cases of functors such as monads and applicative functors
		are some of the most commonly used abstractions in Cats.
		
		3.1 Examples of Functors
		
		Informally, a functor is anything with a map method. You probably know lots
		of types that have this: Option, List, and Either, to name a few.
		
		We typically first encounter map when iterating over Lists. However, to understand
		functors we need to think of the method in another way. Rather than
		traversing the list, we should think of it as transforming all of the values inside
		in one go. We specify the function to apply, and map ensures it is applied to
		every item. The values change but the structure of the list remains the same:
	*/
			List(1, 2, 3).map(n => n + 1)
                                                  
	/*
		
				List[A]      .map (A => B) : List[B]
		
				Option[A]    .map	(A => B) : Option[B]
		
				Either[E, A] .map (A => B) : Either[E, B]
				
				Figure 3.1: Type chart: mapping over List, Option, and Either

		Similarly, when we map over an Option, we transform the contents but leave
		the Some or None context unchanged. The same principle applies to Either
		with its Left and Right contexts. This general notion of transformation, along
		with the common pattern of type signatures shown in Figure 3.1, is what connects
		the behaviour of map across different data types.

		Because map leaves the structure of the context unchanged, we can call it
		repeatedly to sequence multiple computations on the contents of an initial
		data structure:
	*/
	
		List(1, 2, 3).
		map(n => n + 1).
		map(n => n * 2).
		map(n => n + "!")                 

	/*
		We should think of map not as an iteration pattern, but as a way of sequencing
		computations on values ignoring some complication dictated by the relevant
		data type:

					Future[A]   .map (A => B) : Future[B]
					
					Figure 3.2: Type chart: mapping over a Future

			• Option—the value may or may not be present;
			• Either—there may be a value or an error;
			• List—there may be zero or more values.

		3.2 More Examples of Functors

		The map methods of List, Option, and Either apply functions eagerly. However,
		the idea of sequencing computations is more general than this. Let’s
		investigate the behaviour of some other functors that apply the pattern in
		different ways.

		Futures

		Future is a functor that sequences asynchronous computations by queueing
		them and applying them as their predecessors complete. The type signuture
		of its map method, shown in Figure 3.2, has the same shape as the signatures
		above. However, the behaviour is very different.

		When we work with a Future we have no guarantees about its internal state.
		The wrapped computation may be ongoing, complete, or rejected. If the Future
		is complete, our mapping function can be called immediately. If not,
		some underlying thread pool queues the function call and comes back to it
		later. We don’t know when our functions will be called, but we do know what
		order they will be called in. In this way, Future provides the same sequencing
		behaviour seen in List, Option, and Either:
	*/
	
		import scala.concurrent.{Future, Await}
		import scala.concurrent.ExecutionContext.Implicits.global
		import scala.concurrent.duration._
	
		val future: Future[String] =
		Future(123).
		map(n => n + 1).
		map(n => n * 2).
		map(n => n + "!")                 
	
		Await.result(future, 1.second)    

  /*
		Futures and Referential Transparency
		Note that Scala’s Futures aren’t a great example of pure functional programming
		because they aren’t referentially transparent. Future always
		computes and caches a result and there’s no way for us to tweak this
		behaviour. This means we can get unpredictable results when we use
		Future to wrap side-effecting computations. For example:
  */

		import scala.util.Random

		val future1 = {
			// Initialize Random with a fixed seed:
			val r = new Random(0L)
			
			// nextInt has the side-effect of moving to
			// the next random number in the sequence:
			val x = Future(r.nextInt)
			
			for {
				a <- x
				b <- x
			} yield (a, b)
		}                                 

		val future2 = {
			val r = new Random(0L)
	
			for {
				a <- Future(r.nextInt)
				b <- Future(r.nextInt)
			} yield (a, b)
		}                                 

		val result1 = Await.result(future1, 1.second)
                                                  

		val result2 = Await.result(future2, 1.second)
                                                  

  /*
		Ideally we would like result1 and result2 to contain the same value.
		However, the computation for future1 calls nextInt once and the
		computation for future2 calls it twice. Because nextInt returns a different
		result every time we get a different result in each case.

		This kind of discrepancy makes it hard to reason about programs involving
		Futures and side-effects. There also are other problematic aspects
		of Future's behaviour, such as the way it always starts computations
		immediately rather than allowing the user to dictate when the program
		should run. For more information see this excellent Stack Overflow an
		swer by Rob Norris. (https://www.reddit.com/r/scala/comments/3zofjl/why_is_future_totally_unusable/)
 */

/*

					(X => A) . map(A => B) : X => B
			
					Figure 3.3: Type chart: mapping over a Function1

		If Future isn’t referentially transparent, perhaps we should look at another
		similar data-type that is. You should recognise this one…

		Functions (?!)

		It turns out that single argument functions are also functors. To see this we
		have to tweak the types a little. A function A => B has two type parameters:
		the parameter type A and the result type B. To coerce them to the correct
		shape we can fix the parameter type and let the result type vary:

			• start with X => A;
			• supply a function A => B;
			• get back X => B.

		If we alias X => A as MyFunc[A], we see the same pattern of types we saw
		with the other examples in this chapter. We also see this in Figure 3.3:

			• start with MyFunc[A];
			• supply a function A => B;
			• get back MyFunc[B].

		In other words, “mapping” over a Function1 is function composition:

 */

		import cats.instances.function._ // for Functor
		import cats.syntax.functor._ // for map
 
  
		val func1: (Int => Double) =
				(x: Int) => x.toDouble
                                                  

	
		val func2: (Double => Double) =
			(y: Double) => y * 2      

		
		(func1 map func2)(1) // composition using map
                                                  

		(func1 andThen func2)(1) // composition using andThen
                                                  

		func2(func1(1)) // composition written out by hand
                                                  
				
	/*
		How does this relate to our general pattern of sequencing operations? If we
		think about it, function composition is sequencing. We start with a function
		that performs a single operation and every time we use map we append another
		operation to the chain. Calling map doesn’t actually run any of the opera
		tions, but if we can pass an argument to the final function all of the operations
		are run in sequence. We can think of this as lazily queueing up operations similar
		to Future:
	*/
	
		val func =
			((x: Int) => x.toDouble).
				map(x => x + 1).
				map(x => x * 2).
				map(x => x + "!") 

		func(123)                         


  /*
		Partial Unification

		For the above examples to work we need to add the following compiler
		option to build.sbt:

				scalacOptions += "-Ypartial-unification"
		
		otherwise we’ll get a compiler error:

		func1.map(func2)
		// <console>: error: value map is not a member of Int => Double
		// func1.map(func2)
		^
		We’ll look at why this happens in detail in Section 3.8.



					F[A] . map( A => B ) : F[B]
			
					Figure 3.4: Type chart: generalised functor map

		3.3 Definition of a Functor

		Every example we’ve looked at so far is a functor: a class that encapsulates
		sequencing computations. Formally, a functor is a type F[A] with an operation
		map with type (A => B) => F[B]. The general type chart is shown in Figure
		3.4.
		
		Cats encodes Functor as a type class, cats.Functor, so the method looks a
		little different. It accepts the initial F[A] as a parameter alongside the transforma
		tion function. Here’s a simplified version of the definition:
		
				// package cats

				import scala.language.higherKinds

				trait Functor[F[_]] {
					def map[A, B](fa: F[A])(f: A => B): F[B]
				}
		
		If you haven’t seen syntax like F[_] before, it’s time to take a brief detour
		to discuss type constructors and higher kinded types. We’ll explain that
		scala.language import as well.
	

		Functor Laws
		
		Functors guarantee the same semantics whether we sequence many
		small operations one by one, or combine them into a larger function
		before mapping. To ensure this is the case the following laws must hold:
		
		Identity: calling map with the identity function is the same as doing nothing:
		
				fa.map(a => a) == fa
		
		Composition: mapping with two functions f and g is the same as mapping
		with f and then mapping with g:
		
				fa.map(g(f(_))) == fa.map(f).map(g)
		
		
		3.4 Aside: Higher Kinds and Type Constructors
		
		Kinds are like types for types. They describe the number of “holes” in a type.
		We distinguish between regular types that have no holes and “type constructors”
		that have holes we can fill to produce types.
		
		For example, List is a type constructor with one hole. We fill that hole by
		specifying a parameter to produce a regular type like List[Int] or List[A].
		The trick is not to confuse type constructors with generic types. List is a type
		constructor, List[A] is a type:
		
				List // type constructor, takes one parameter
				List[A] // type, produced using a type parameter
		
		There’s a close analogy here with functions and values. Functions are “value
		constructors”—they produce values when we supply parameters:
		
				math.abs // function, takes one parameter
				math.abs(x) // value, produced using a value parameter

		In Scala we declare type constructors using underscores. Once we’ve declared
		them, however, we refer to them as simple identifiers:

				// Declare F using underscores:
				def myMethod[F[_]] = {

					// Reference F without underscores:
					val functor = Functor.apply[F]

					// ...
				}
		
		This is analogous to specifying a function’s parameters in its definition and
		omiting them when referring to it:

				// Declare f specifying parameters:
				val f = (x: Int) => x * 2
				
				// Reference f without parameters:
				val f2 = f andThen f
		
		Armed with this knowledge of type constructors, we can see that the Cats defini
		tion of Functor allows us to create instances for any single-parameter type
		constructor, such as List, Option, Future, or a type alias such as MyFunc.

		Language Feature Imports

		Higher kinded types are considered an advanced language feature in
		Scala. Whenever we declare a type constructor with A[_] syntax, we
		need to “enable” the higher kinded type language feature to suppress
		warnings from the compiler. We can either do this with a “language
		import” as above:

				import scala.language.higherKinds
		
		or by adding the following to scalacOptions in build.sbt:
		
				scalacOptions += "-language:higherKinds"
		
		We’ll use the language import in this book to ensure we are as explicit
		as possible. In practice, however, we find the scalacOptions flag to
		be simpler and less verbose.
		
		3.5 Functors in Cats
		
		Let’s look at the implementation of functors in Cats. We’ll examine the aspects
		we did for monoids: the type class, the instances, and the syntax.
		
		3.5.1 The Functor Type Class
		
		The functor type class is cats.Functor. We obtain instances using the standard
		Functor.apply method on the companion object. As usual, default instances
		are arranged by type in the cats.instances // package:
  */

		import scala.language.higherKinds
		import cats.Functor
		import cats.instances.list._ // for Functor
		import cats.instances.option._ // for Functor
		
		val list1 = List(1, 2, 3)         

		val list2 = Functor[List].map(list1)(_ * 2)
                                                  

		val option1 = Option(123)         

		val option2 = Functor[Option].map(option1)(_.toString)
                                                  

	/*
		Functor also provides the lift method, which converts a function of type A
		=> B to one that operates over a functor and has type F[A] => F[B]:
	*/
	
		val func3 = (x: Int) => x + 1     


		val liftedFunc = Functor[Option].lift(func3)
                                                  


		liftedFunc(Option(1))             


  /*
		3.5.2 Functor Syntax
	
		The main method provided by the syntax for Functor is map. It’s difficult to
		demonstrate this with Options and Lists as they have their own built-in map
		methods and the Scala compiler will always prefer a built-in method over an
		extension method. We’ll work around this with two examples.

		First let’s look at mapping over functions. Scala’s Function1 type doesn’t
		have a map method (it’s called andThen instead) so there are no naming conflicts:
 */
 
		import cats.instances.function._ // for Functor
		import cats.syntax.functor._ // for map

		val func4 = (a: Int) => a + 1     

		val func5 = (a: Int) => a * 2     

		val func6 = (a: Int) => a + "!"   

		val func7 = func4.map(func5).map(func6)
                                                  

		func7(123)                        

  /*
		Let’s look at another example. This time we’ll abstract over functors so we’re
		not working with any particular concrete type. We can write a method that
		applies an equation to a number no matter what functor context it’s in:
  */
  
		def doMath[F[_]](start: F[Int])
			(implicit functor: Functor[F]): F[Int] =
			start.map(n => n + 1 * 2) 

		import cats.instances.option._ // for Functor
		import cats.instances.list._ // for Functor
	
		doMath(Option(20))                
	
		doMath(List(1, 2, 3))             

	/*
		To illustrate how this works, let’s take a look at the definition of the map
		method in cats.syntax.functor. Here’s a simplified version of the code:
		
				implicit class FunctorOps[F[_], A](src: F[A]) {
					def map[B](func: A => B)
							(implicit functor: Functor[F]): F[B] =
						functor.map(src)(func)
				}
	
		The compiler can use this extension method to insert a map method wherever
		no built-in map is available:
		
				foo.map(value => value + 1)
		
		Assuming foo has no built-in map method, the compiler detects the potential
		error and wraps the expression in a FunctorOps to fix the code:
		
				new FunctorOps(foo).map(value => value + 1)
		
		The map method of FunctorOps requires an implicit Functor as a parameter.
		This means this code will only compile if we have a Functor for expr1 in
		scope. If we don’t, we get a compiler error:
		
				final case class Box[A](value: A)

				val box = Box[Int](123)
				box.map(value => value + 1)
				// <console>:34: error: value map is not a member of Box[Int]
				// box.map(value => value + 1)
				//     ^
				
		3.5.3 Instances for Custom Types

		We can define a functor simply by defining its map method. Here’s an example
		of a Functor for Option, even though such a thing already exists in
		cats.instances. The implementation is trivial—we simply call Option's
		map method:

				implicit val optionFunctor: Functor[Option] =
					new Functor[Option] {
						def map[A, B](value: Option[A])(func: A => B): Option[B] =
							value.map(func)
				}

		Sometimes we need to inject dependencies into our instances. For example,
		if we had to define a custom Functor for Future (another hypothetical
		example—Cats provides one in cats.instances.future) we would need to
		account for the implicit ExecutionContext parameter on future.map. We
		can’t add extra parameters to functor.map so we have to account for the
		dependency when we create the instance:
		
		import scala.concurrent.{Future, ExecutionContext}
		
		implicit def futureFunctor
				(implicit ec: ExecutionContext): Functor[Future] =
			new Functor[Future] {
				def map[A, B](value: Future[A])(func: A => B): Future[B] =
					value.map(func)
		}
		
		Whenever we summon a Functor for Future, either directly using Functor.
		apply or indirectly via the map extension method, the compiler will locate
		futureFunctor by implicit resolution and recursively search for an ExecutionContext
		at the call site. This is what the expansion might look like:

				// We write this:
				Functor[Future]

				// The compiler expands to this first:
				Functor[Future](futureFunctor)

				// And then to this:
				Functor[Future](futureFunctor(executionContext))

		3.5.4 Exercise: Branching out with Functors

		Write a Functor for the following binary tree data type. Verify that the code
		works as expected on instances of Branch and Leaf:
  */
		sealed trait Tree[+A]

		final case class Branch[A](left: Tree[A], right: Tree[A])
			extends Tree[A]
	
		final case class Leaf[A](value: A) extends Tree[A]
  /*
		See the solution
  	----------------------- solution ------------------------------
  */
	/*
		C.1 Branching out with Functors
		The semantics are similar to writing a Functor for List. We recurse over the
		data structure, applying the function to every Leaf we find. The functor laws
		intuitively require us to retain the same structure with the same pattern of
		Branch and Leaf nodes:
	*/

	import cats.Functor
	
	implicit val treeFunctor: Functor[Tree] =
		new Functor[Tree] {
			def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
				tree match {
					case Branch(left, right) =>
					Branch(map(left)(func), map(right)(func))
					case Leaf(value) =>
					Leaf(func(value))
				}
		}                                 


	/*
		Let’s use our Functor to transform some Trees:
	*/

	// Branch(Leaf(10), Leaf(20)).map(_ * 2)
	// <console>:42: error: value map is not a member of wrapper.Branch[Int]
  // Branch(Leaf(10), Leaf(20)).map(_ * 2)
  //                            ^

	/*
		Oops! This is falls foul of the same invariance problem we discussed in Section
		1.6.1. The compiler can find a Functor instance for Tree but not for Branch
		or Leaf. Let’s add some smart constructors to compensate:
	*/

	object Tree {
		def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
			Branch(left, right)

		def leaf[A](value: A): Tree[A] =
			Leaf(value)
	}

	/*
		Now we can use our Functor properly:
	*/

	Tree.leaf(100).map(_ * 2)                 

	Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)
                                                  


  /*
    ---------------------------------------------------------------
		3.6 Contravariant and Invariant Functors
	
		As we have seen, we can think of Functor's map method as “appending” a
		transformation to a chain. We’re now going to look at two other type classes,
		one representing prepending operations to a chain, and one representing building
		a bidirectional chain of operations. These are called contravariant and invariant
		functors respectively.
	
		This Section is Optional!
		
		You don’t need to know about contravariant and invariant functors to
		understand monads, which are the most important pattern in this book
		and the focus of the next chapter. However, contravariant and invariant

					F[B]  . contramap(A => B) :  F[A]
					
					Figure 3.5: Type chart: the contramap method

		do come in handy in our discussion of Semigroupal and Applicative
		in Chapter 6.
		
		If you want to move on to monads now, feel free to skip straight to
		Chapter 4. Come back here before you read Chapter 6.
		
		3.6.1 Contravariant Functors and the contramap Method
		
		The first of our type classes, the contravariant functor, provides an operation
		called contramap that represents “prepending” an operation to a chain. The
		general type signature is shown in Figure 3.5.
		
		The contramap method only makes sense for data types that represent transforma
		tions. For example, we can’t define contramap for an Option because
		there is no way of feeding a value in an Option[B] backwards through a func-
		tion A => B. However, we can define contramap for the Printable type
		class we discussed in Chapter 1:
		
					trait Printable[A] {
						def format(value: A): String
					}

		A Printable[A] represents a transformation from A to String. Its contramap
		method accepts a function func of type B => A and creates a new
		Printable[B]:

					trait Printable[A] {
						def format(value: A): String
						
						def contramap[B](func: B => A): Printable[B] =
							???
					}
				
					def format[A](value: A)(implicit p: Printable[A]): String =
						p.format(value)

		3.6.1.1 Exercise: Showing off with Contramap

		Implement the contramap method for Printable above. Start with the following
		code template and replace the ??? with a working method body:

					trait Printable[A] {
						def format(value: A): String

						def contramap[B](func: B => A): Printable[B] =
							new Printable[B] {
								def format(value: B): String =
									???
						}
					}
		
		If you get stuck, think about the types. You need to turn value, which is of
		type B, into a String. What functions and methods do you have available and
		in what order do they need to be combined?

		See the solution
		----------------------- solution ------------------------------
    
	    C.2 Showing off with Contramap
	
			Here’s a working implementation. We call func to turn the B into an A and
			then use our original Printable to turn the A into a String. In a small
			show of sleight of hand we use a self alias to distinguish the outer and inner
			Printables:
	*/

		trait Printable[A] {
			self =>
				def format(value: A): String

				def contramap[B](func: B => A): Printable[B] =
					new Printable[B] {
						def format(value: B): String =
							self.format(func(value))
			}
		}
		
		def format[A](value: A)(implicit p: Printable[A]): String =
			p.format(value)           

		
  /*
    ---------------------------------------------------------------

		For testing purposes, let’s define some instances of Printable for String
		and Boolean:
	*/
		implicit val stringPrintable: Printable[String] =
			new Printable[String] {
				def format(value: String): String =
					"\"" + value + "\""
		}                                 

		
		implicit val booleanPrintable: Printable[Boolean] =
			new Printable[Boolean] {
				def format(value: Boolean): String =
					if(value) "yes" else "no"
				}                 


		format("hello")                   

		format(true)                      

	/*
		Now define an instance of Printable for the following Box case class. You’ll
		need to write this as an implicit def as described in Section 1.2.3:
	*/
	
		final case class Box[A](value: A)
	
	/*
		Rather than writing out the complete definition from scratch (new Printable[
		Box] etc…), create your instance from an existing instance using contramap.
		See the solution
		
		Your instance should work as follows:
		----------------------- solution ------------------------------

		C.3 Showing off with Contramap Part 2
		
		To make the instance generic across all types of Box, we base it on the
		Printable for the type inside the Box. We can either write out the complete
		definition by hand:
  */

/*
		implicit def boxPrintable[A](implicit p: Printable[A]) =
			new Printable[Box[A]] {
				def format(box: Box[A]): String =
					p.format(box.value)
		}
*/

	/*
		or use contramap to base the new instance on the implicit parameter:
	*/

		implicit def boxPrintable[A](implicit p: Printable[A]) =
			p.contramap[Box[A]](_.value)
                                                  


	/*
		Using contramap is much simpler, and conveys the functional programming
		approach of building solutions by combining simple building blocks using pure
		functional combinators.
	*/

  /*
    ---------------------------------------------------------------
	*/

		format(Box("hello world"))        
		// res5: String = "hello world"
		format(Box(true))                 
		// res6: String = yes
	/*
		If we don’t have a Printable for the type inside the Box, calls to format
		should fail to compile:
	*/

		// format(Box(123))
		// <console>:21: error: could not find implicit value for parameter p:
		// Printable[Box[Int]]
		// format(Box(123))
		//       ^

	/*
					F[A]   . imap( A => B, B => A) : F[B]
			
					Figure 3.6: Type chart: the imap method
		
		3.6.2 Invariant functors and the imap method

		Invariant functors implement a method called imap that is informally equivalent
		to a combination of map and contramap. If map generates new type class
		instances by appending a function to a chain, and contramap generates them
		by prepending an operation to a chain, imap generates them via a pair of bidirec
		tional transformations.
		
		The most intuitive examples of this are a type class that represents encoding
		and decoding as some data type, such as Play JSON’s Format and scodec’s
		Codec. We can build our own Codec by enhancing Printable to support
		encoding and decoding to/from a String:
		
		trait Codec[A] {
			def encode(value: A): String
			def decode(value: String): A
			def imap[B](dec: A => B, enc: B => A): Codec[B] = ???
		}
		
		def encode[A](value: A)(implicit c: Codec[A]): String =
			c.encode(value)
	
		def decode[A](value: String)(implicit c: Codec[A]): A =
			c.decode(value)
		
		The type chart for imap is shown in Figure 3.6. If we have a Codec[A] and a
		pair of functions A => B and B => A, the imap method creates a Codec[B]:

		As an example use case, imagine we have a basic Codec[String], whose
		encode and decode methods are both a no-op:


		implicit val stringCodec: Codec[String] =
			new Codec[String] {
				def encode(value: String): String = value
				def decode(value: String): String = value
			}

		We can construct many useful Codecs for other types by building off of
		stringCodec using imap:

		implicit val intCodec: Codec[Int] =
			stringCodec.imap(_.toInt, _.toString)

		implicit val booleanCodec: Codec[Boolean] =
			stringCodec.imap(_.toBoolean, _.toString)

		Coping with Failure
	
		Note that the decode method of our Codec type class doesn’t account
		for failures. If we want to model more sophisticated relationships we
		can look move beyond functors to look at lenses and optics.
	
		Optics are beyond the scope of this book. However, Julien Truffaut’s
		library Monocle provides a great starting point for further investigation.
	
		3.6.2.1 Transformative Thinking with imap
	
		Implement the imap method for Codec above.
		See the solution

			sandbox.functor.Printable
  	----------------------- solution ------------------------------
			C.4 Transformative Thinking with imap
			Here’s a working implementation:
	*/

			trait Codec[A] {
				def encode(value: A): String
				def decode(value: String): A

				def imap[B](dec: A => B, enc: B => A): Codec[B] = {
					val self = this
					new Codec[B] {
						def encode(value: B): String =
							self.encode(enc(value))
	
						def decode(value: String): B =
							dec(self.decode(value))
					}
				}
			}


  /*
    ---------------------------------------------------------------

		Demonstrate your imap method works by creating a Codec for Double.

		See the solution
			sandbox.functor.Printable
  	----------------------- solution ------------------------------
	
		C.5 Transformative Thinking with imap Part 2

		We can implement this using the imap method of stringCodec:
	*/

	object CodecInstances {
	
		implicit val stringCodec: Codec[String] =
			new Codec[String] {
				def encode(value: String): String = value
				def decode(value: String): String = value
			}
			
		implicit val intCodec: Codec[Int] =
			stringCodec.imap(_.toInt, _.toString)

		implicit val booleanCodec: Codec[Boolean] =
			stringCodec.imap(_.toBoolean, _.toString)

			
		implicit val doubleCodec: Codec[Double] =
			stringCodec.imap[Double](_.toDouble, _.toString)

	/*
    ---------------------------------------------------------------
		Finally, implement a Codec for the following Box type:
					case class Box[A](value: A)

		See the solution

  	----------------------- solution ------------------------------

		C.6 Transformative Thinking with imap Part 3

		We need a generic Codec for Box[A] for any given A. We create this by calling
		imap on a Codec[A], which we bring into scope using an implicit parameter:
	*/

		implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
				c.imap[Box[A]](Box(_), _.value)

	}
  /*
    ---------------------------------------------------------------

		Your instances should work as follows:

  */


		object Codec {
		  def encode[A](value: A)(implicit c: Codec[A]): String =
		    c.encode(value)
		  def decode[A](value: String)(implicit c: Codec[A]): A =
		    c.decode(value)
		}


		import CodecInstances._
		import Codec._

		encode(123.4)                     


		decode[Double]("123.4")           

		encode(Box(123.4))                

		decode[Box[Double]]("123.4")      


  /*
		What’s With the Names?

		What’s the relationship between the terms “contravariance”, “invariance”,
		and “covariance” and these different kinds of functor?

		If you recall from Section 1.6.1, variance affects subtyping, which is essen
		tially our ability to use a value of one type in place of a value of
		another type without breaking the code.

		Subtyping can be viewed as a conversion. If B is a subtype of A, we can
		always convert a B to an A.

		Equivalently we could say that B is a subtype of A if there exists a func-
		tion A => B. A standard covariant functor captures exactly this. If F is
		a covariant functor, wherever we have an F[A] and a conversion A =>
		B we can always convert to an F[B].

		A contravariant functor captures the opposite case. If F is a contravariant
		functor, whenever we have a F[A] and a conversion B => A we can
		convert to an F[B].

		Finally, invariant functors capture the case where we can convert from
		F[A] to F[B] via a function A => B and vice versa via a function B =>
		A.


		3.7 Contravariant and Invariant in Cats

		Let’s look at the implementation of contravariant and invariant functors in Cats,
		provided by the cats.Contravariant and cats.Invariant type classes.
		Here’s a simplified version of the code:
		
		trait Contravariant[F[_]] {
			def contramap[A, B](fa: F[A])(f: B => A): F[B]
		}
		
		trait Invariant[F[_]] {
			def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
		}
		
		3.7.1 Contravariant in Cats
		
		We can summon instances of Contravariant using the Contravariant.
		apply method. Cats provides instances for data types that consume
		parameters, including Eq, Show, and Function1. Here’s an example:
	*/
	
		import cats.Contravariant
		import cats.Show
		import cats.instances.string._
		
		val showString = Show[String]     
		
		val showSymbol = Contravariant[Show].
			contramap(showString)((sym: Symbol) => s"'${sym.name}")
                                                  
		
		showSymbol.show('dave)            
	
	/*
		More conveniently, we can use cats.syntax.contravariant, which provides
		a contramap extension method:
	*/
	
		import cats.syntax.contravariant._ // for contramap

		showString.contramap[Symbol](_.name).show('dave)
                                                  

	/*
		3.7.2 Invariant in Cats

		Among other types, Cats provides an instance of Invariant for Monoid. This
		is a little different from the Codec example we introduced in Section 3.6.2. If
		you recall, this is what Monoid looks like:
		
				// package cats

				trait Monoid[A] {
					def empty: A
					def combine(x: A, y: A): A
				}

		Imagine we want to produce a Monoid for Scala’s Symbol type. Cats doesn’t
		provide a Monoid for Symbol but it does provide a Monoid for a similar type:
		String. We can write our new semigroup with an empty method that relies
		on the empty String, and a combine method that works as follows:

			1. accept two Symbols as parameters;
			2. convert the Symbols to Strings;
			3. combine the Strings using Monoid[String];
			4. convert the result back to a Symbol.
			
		We can implement combine using imap, passing functions of type String =>
		Symbol and Symbol => String as parameters. Here’ the code, written out
		using the imap extension method provided by cats.syntax.invariant:
	*/
		
		import cats.Monoid
		import cats.instances.string._ // for Monoid
		import cats.syntax.invariant._ // for imap
		import cats.syntax.semigroup._ // for |+|

		implicit val symbolMonoid: Monoid[Symbol] =
			Monoid[String].imap(Symbol.apply)(_.name)
                                                  

	
		Monoid[Symbol].empty              

		'a |+| 'few |+| 'words            

	/*
		3.8 Aside: Partial Unification

		In Section 3.2 we saw a curious compiler error. The following code compiled
		perfectly if we had the -Ypartial-unification compiler flag enabled:

	*/
	
		import cats.Functor
		import cats.instances.function._ // for Functor
		import cats.syntax.functor._ // for map
	
		val func8 = (x: Int) => x.toDouble

		val func9 = (y: Double) => y * 2  

		
		val func10 = func1.map(func9)     

	/*
		but failed if the flag was missing:
		val func11 = func8.map(func9)
		// <console>: error: value map is not a member of Int => Double
		// val func11 = func1.map(func9)
		                     ^
	*/

/*
		Obviously “partial unification” is some kind of optional compiler behaviour,
		without which our code will not compile. We should take a moment to describe
		this behaviour and discuss some gotchas and workarounds.
		
		3.8.1 Unifying Type Constructors
		
		In order to compile an expression like func1.map(func2) above, the compiler
		has to search for a Functor for Function1. However, Functor accepts a type
		constructor with one parameter:

					trait Functor[F[_]] {
						def map[A, B](fa: F[A])(func: A => B): F[B]
					}
	
		and Function1 has two type parameters (the function argument and the result
		type):

					trait Function1[-A, +B] {
						def apply(arg: A): B
					}
	
		The compiler has to fix one of the two parameters of Function1 to create a
		type constructor of the correct kind to pass to Functor. It has two options to
		choose from:
	
					type F[A] = Int => A
					type F[A] = A => Double
	
		We know that the former of these is the correct choice. However, earlier versions
		of the Scala compiler were not able to make this inference. This infamous
		limitation, known as SI-2712, prevented the compiler “unifying” type constructors
		of different arities. This compiler limitation is now fixed, although we have
		to enable the fix via a compiler flag in build.sbt:

					scalacOptions += "-Ypartial-unification"

		3.8.2 Left-to-Right Elimination
		
		The partial unification in the Scala compiler works by fixing type parameters
		from left to right. In the above example, the compiler fixes the Int in Int =>
		Double and looks for a Functor for functions of type Int => ?:
		
				type F[A] = Int => A
				val functor = Functor[F]

		This left-to-right elimination works for a wide variety of common scenarios,
		including Functors for types such as Function1 and Either:
	*/

		import cats.instances.either._ // for Functor

		val either: Either[String, Int] = Right(123)
                                                  

		either.map(_ + 1)                 
	/*
		However, there are situations where left-to-right elimination is not the correct
		choice. One example is the Or type in Scalactic, which is a conventionally left-
		biased equivalent of Either:
	
	
					type PossibleResult = ActualResult Or Error

	
		Another example is the Contravariant functor for Function1.
		While the covariant Functor for Function1 implements andThen-style left-
		to-right function composition, the Contravariant functor implements compose-
		style right-to-left composition. In other words, the following expressions
		are all equivalent:
	*/
		val func3a: Int => Double =
			a => func2(func1(a))      


		val func3b: Int => Double =
			func2.compose(func1)      

	/*
		// Hypothetical example. This won't actually compile:
		val func3c: Int => Double =
			func2.contramap(func1)
	
		If we try this for real, however, our code won’t compile:
	
		import cats.syntax.contravariant._ // for contramap
		val func3c = func2.contramap(func1)

		// <console>:27: error: value contramap is not a member of Double =>
		Double
		// val func3c = func2.contramap(func1)
		// ^

		(A => X)   .contramap( B => A ) : B => X
		
		Figure 3.7: Type chart: contramapping over a Function1

		The problem here is that the Contravariant for Function1 fixes the return
		type and leaves the parameter type varying, requiring the compiler to eliminate
		type parameters from right to left, as shown below and in Figure 3.7:
		
					type F[A] = A => Double

		The compiler fails simply because of its left-to-right bias. We can prove this
		by creating a type alias that flips the parameters on Function1:

					type <=[B, A] = A => B
					type F[A] = Double <= A

		If we re-type func2 as an instance of <=, we reset the required order of elimina
		tion and we can call contramap as desired:
		
					val func2b: Double <= Double = func2
					val func3c = func2b.contramap(func1)
					// func3c: Double <= Int = scala.runtime.
					AbstractFunction1$$Lambda$2313/1769218156@656e1369
		
		The difference between func2 and func2b is purely syntactic—both refer to
		the same value and the type aliases are otherwise completely compatible. Incredibly,
		however, this simple rephrasing is enough to give the compiler the
		hint it needs to solve the problem.

		It is rare that we have to do this kind of right-to-left elimination. Most multi-
		parameter type constructors are designed to be right-biased, requiring the left-
		to-right elimination that is supported by the compiler out of the box. However,
		it is useful to know about -Ypartial-unification and this quirk of elimina-
		tion order in case you ever come across an odd scenario like the one above.

		3.9 Summary

		Functors represent sequencing behaviours. We covered three types of functor
		in this chapter:

			• Regular covariant Functors, with their map method, represent the ability
			to apply functions to a value in some context. Successive calls to
			map apply these functions in sequence, each accepting the result of its
			predecessor as a parameter.
	
			• Contravariant functors, with their contramap method, represent the
			ability to “prepend” functions to a function-like context. Successive
			calls to contramap sequence these functions in the opposite order to
			map.
	
			• Invariant functors, with their imap method, represent bidirectional
			transformations.

		Regular Functors are by far the most common of these type classes, but even
		then it is rare to use them on their own. Functors form a foundational building
		block of several more interesting abstractions that we use all the time. In
		the following chapters we will look at two of these abstractions: monads and
		applicative functors.

		Functors for collections are extremely important, as they transform each element
		independently of the rest. This allows us to parallelise or distribute
		transformations on large collections, a technique leveraged heavily in “mapreduce”
		frameworks like Hadoop. We will investigate this approach in more
		detail in the Map-reduce case study later in the book.

		The Contravariant and Invariant type classes are less widely applicable
		but are still useful for building data types that represent transformations. We
		will revisit them to discuss the Semigroupal type class later in Chapter 6.
		
	*/


// }
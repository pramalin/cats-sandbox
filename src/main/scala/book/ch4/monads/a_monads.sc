package book.ch4.monads

object a_monads {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  /*
	  Chapter 4

		Monads

		Monads are one of the most common abstractions in Scala. Many Scala programmers
		quickly become intuitively familiar with monads, even if we don’t
		know them by name.

		Informally, a monad is anything with a constructor and a flatMap method. All
		of the functors we saw in the last chapter are also monads, including Option,
		List, and Future. We even have special syntax to support monads: for comprehensions.
		However, despite the ubiquity of the concept, the Scala standard
		library lacks a concrete type to encompass “things that can be flatMapped”.
		This type class is one of the benefits bought to us by Cats.
		
		In this chapter we will take a deep dive into monads. We will start by moti-
		vating them with a few examples. We’ll proceed to their formal definition and
		their implementation in Cats. Finally, we’ll tour some interesting monads that
		you may not have seen, providing introductions and examples of their use.
		
		4.1 What is a Monad?
		
		This is the question that has been posed in a thousand blog posts, with explana
		tions and analogies involving concepts as diverse as cats, Mexican food,
		space suits full of toxic waste, and monoids in the category of endofunctors
		(whatever that means). We’re going to solve the problem of explaining monads
		once and for all by stating very simply:

				A monad is a mechanism for sequencing computations.
		
		That was easy! Problem solved, right? But then again, last chapter we said
		functors were a control mechanism for exactly the same thing. Ok, maybe we
		need some more discussion…
		
		In Section 3.1 we said that functors allow us to sequence computations ignoring
		some complication. However, functors are limited in that they only allow
		this complication to occur once at the beginning of the sequence. They don’t
		account further complications at each step in the sequence.
		
		This is where monads come in. A monad’s flatMap method allows us to specify
		what happens next, taking into account an intermediate complication. The
		flatMap method of Option takes intermediate Options into account. The
		flatMap method of List handles intermediate Lists. And so on. In each
		case, the function passed to flatMap specifies the application-specific part
		of the computation, and flatMap itself takes care of the complication allowing
		us to flatMap again. Let’s ground things by looking at some examples.

		Options

		Option allows us to sequence computations that may or may not return values.
		Here are some examples:
	*/

		def parseInt(str: String): Option[Int] =
			scala.util.Try(str.toInt).toOption
                                                  //> parseInt: (str: String)Option[Int]
		
		def divide(a: Int, b: Int): Option[Int] =
			if(b == 0) None else Some(a / b)
                                                  //> divide: (a: Int, b: Int)Option[Int]

	/*
		Each of these methods may “fail” by returning None. The flatMap method
		allows us to ignore this when we sequence operations:
	*/
	
		def stringDivideBy1(aStr: String, bStr: String): Option[Int] =
			parseInt(aStr).flatMap { aNum =>
				parseInt(bStr).flatMap { bNum =>
					divide(aNum, bNum)
				}
			}                         //> stringDivideBy1: (aStr: String, bStr: String)Option[Int]

	/*

					Option[A]    .flatMap(A => Option[B])  :    Option[B]
					
					Figure 4.1: Type chart: flatMap for Option

	
		We know the semantics well:

			• the first call to parseInt returns a None or a Some;
			• if it returns a Some, the flatMap method calls our function and passes
			us the integer aNum;
			• the second call to parseInt returns a None or a Some;
			• if it returns a Some, the flatMap method calls our function and passes
			us bNum;
			• the call to divide returns a None or a Some, which is our result.

		At each step, flatMap chooses whether to call our function, and our function
		generates the next computation in the sequence. This is shown in Figure 4.1.
		The result of the computation is an Option, allowing us to call flatMap again
		and so the sequence continues. This results in the fail-fast error handling behaviour
		that we know and love, where a None at any step results in a None
		overall:
  */
  
		stringDivideBy1("6", "2")         //> res0: Option[Int] = Some(3)

		stringDivideBy1("6", "0")         //> res1: Option[Int] = None

		stringDivideBy1("6", "foo")       //> res2: Option[Int] = None

		stringDivideBy1("bar", "2")       //> res3: Option[Int] = None

  
  /*
		Every monad is also a functor (see below for proof), so we can rely on both
		flatMap and map to sequence computations that do and don’t introduce a
		new monad. Plus, if we have both flatMap and map we can use for comprehensions
		to clarify the sequencing behaviour:
	*/
	
		def stringDivideBy(aStr: String, bStr: String): Option[Int] =
			for {
				aNum <- parseInt(aStr)
				bNum <- parseInt(bStr)
				ans <- divide(aNum, bNum)
			} yield ans               //> stringDivideBy: (aStr: String, bStr: String)Option[Int]

	/*
		Lists

		When we first encounter flatMap as budding Scala developers, we tend to
		think of it as a pa􀂂ern for iterating over Lists. This is reinforced by the syntax
		of for comprehensions, which look very much like imperative for loops:
	*/
			
		for {
			x <- (1 to 3).toList
			y <- (4 to 5).toList
		} yield (x, y)
	
	/*
		However, there is another mental model we can apply that highlights the
		monadic behaviour of List. If we think of Lists as sets of intermediate results,
		flatMap becomes a construct that calculates permutations and combina
		tions.
		
		For example, in the for comprehension above there are three possible values
		of x and two possible values of y. This means there are six possible values
		of (x, y). flatMap is generating these combinations from our code, which
		states the sequence of operations:

			• get x
			• get y
			• create a tuple (x, y)

		Futures

		Future is a monad that sequences computations without worrying that they
		are asynchronous:

					import scala.concurrent.Future
					import scala.concurrent.ExecutionContext.Implicits.global
					import scala.concurrent.duration._
			
					def doSomethingLongRunning: Future[Int] = ???
					def doSomethingElseLongRunning: Future[Int] = ???
			
					def doSomethingVeryLongRunning: Future[Int] =
						for {
							result1 <- doSomethingLongRunning
							result2 <- doSomethingElseLongRunning
						} yield result1 + result2

		Again, we specify the code to run at each step, and flatMap takes care of all
		the horrifying underlying complexities of thread pools and schedulers.

		If you’ve made extensive use of Future, you’ll know that the code above is
		running each operation in sequence. This becomes clearer if we expand out
		the for comprehension to show the nested calls to flatMap:

					def doSomethingVeryLongRunning: Future[Int] =
						doSomethingLongRunning.flatMap { result1 =>
							doSomethingElseLongRunning.map { result2 =>
								result1 + result2
						}
					}

					Future[A]     .flatMap( A => Future[B] )     :   Future[B]

					Figure 4.2: Type chart: flatMap for Future

		Each Future in our sequence is created by a function that receives the result
		from a previous Future. In other words, each step in our computation can
		only start once the previous step is finished. This is born out by the type chart
		for flatMap in Figure 4.2, which shows the function parameter of type A =>
		Future[B].

		We can run futures in parallel, of course, but that is another story and shall be
		told another time. Monads are all about sequencing.

		4.1.1 Definition of a Monad

		While we have only talked about flatMap above, monadic behaviour is formally
		captured in two operations:

			• pure, of type A => F[A];
			• flatMap¹, of type (F[A], A => F[B]) => F[B].
		[
		¹In some libraries and languages, notably Scalaz and Haskell, pure is referred to as point or
		return and flatMap is referred to as bind or >>=. This is purely a difference in terminology.
		We’ll use the term flatMap for compatibility with Cats and the Scala standard library.
		]

		pure abstracts over constructors, providing a way to create a new monadic
		context from a plain value. flatMap provides the sequencing step we have
		already discussed, extracting the value from a context and generating the next
		context in the sequence. Here is a simplified version of the Monad type class
		in Cats:

					import scala.language.higherKinds
			
					trait Monad[F[_]] {
						def pure[A](value: A): F[A]
				
						def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
					}

		Monad Laws

		pure and flatMap must obey a set of laws that allow us to sequence
		operations freely without unintended glitches and side-effects:

		Left identity: calling pure and transforming the result with func is the
		same as calling func:

					pure(a).flatMap(func) == func(a)

		Right identity: passing pure to flatMap is the same as doing nothing:
		
					m.flatMap(pure) == m
		
		Associativity: flatMapping over two functions f and g is the same as
		flatMapping over f and then flatMapping over g:
		
					m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
					
					
		4.1.2 Exercise: Getting Func-y
		
		Every monad is also a functor. We can define map in the same way for every
		monad using the existing methods, flatMap and pure:
		
					import scala.language.higherKinds

					trait Monad[F[_]] {
						def pure[A](a: A): F[A]

						def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

						def map[A, B](value: F[A])(func: A => B): F[B] =
							???
					}
		
		Try defining map yourself now.
		See the solution

			----------------------- solution ------------------------------
	
		D.1 Getting Func-y

		At first glance this seems tricky, but if we follow the types we’ll see there’s only
		one solution. We are passed a value of type F[A]. Given the tools available
		there’s only one thing we can do: call flatMap:

		trait Monad[F[_]] {
			def pure[A](value: A): F[A]
	
			def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
	
			def map[A, B](value: F[A])(func: A => B): F[B] =
				flatMap(value)(a => ???)
		}

		We need a function of type A => F[B] as the second parameter. We have two
		function building blocks available: the func parameter of type A => B and the
		pure function of type A => F[A]. Combining these gives us our result:

		trait Monad[F[_]] {
			
			def pure[A](value: A): F[A]
			
			def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
			def map[A, B](value: F[A])(func: A => B): F[B] =
			flatMap(value)(a => pure(func(a)))
		}

	    ---------------------------------------------------------------

	
		4.2 Monads in Cats
	
		It’s time to give monads our standard Cats treatment. As usual we’ll look at
		the type class, instances, and syntax.
		
		4.2.1 The Monad Type Class
		
		The monad type class is cats.Monad. Monad extends two other type classes:
		FlatMap, which provides the flatMap method, and Applicative, which provides
		pure. Applicative also extends Functor, which gives every Monad a
		map method as we saw in the exercise above. We’ll discuss Applicatives in
		
		Chapter 6.
		
		Here are some examples using pure and flatMap, and map directly:
	*/
			
		import cats.Monad
		import cats.instances.option._ // for Monad
		import cats.instances.list._ // for Monad
	
		val opt1 = Monad[Option].pure(3)

		val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))

		val opt3 = Monad[Option].map(opt2)(a => 100 * a)

		val list1 = Monad[List].pure(3)

		val list2 = Monad[List].
			flatMap(List(1, 2, 3))(a => List(a, a*10))

		val list3 = Monad[List].map(list2)(a => a + 123)

  /*

		Monad provides many other methods, including all of the methods from Functor.
		See the scaladoc for more information.

		4.2.2 Default Instances

		Cats provides instances for all the monads in the standard library (Option,
		List, Vector and so on) via cats.instances:
  */

		import cats.instances.option._ // for Monad

		Monad[Option].flatMap(Option(1))(a => Option(a*2))

		import cats.instances.list._ // for Monad

		Monad[List].flatMap(List(1, 2, 3))(a => List(a, a*10))

		import cats.instances.vector._ // for Monad

		Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a*10))

  /*

		Cats also provides a Monad for Future. Unlike the methods on the Future
		class itself, the pure and flatMap methods on the monad can’t accept implicit
		ExecutionContext parameters (because the parameters aren’t part of the
		definitions in the Monad trait). To work around this, Cats requires us to have
		an ExecutionContext in scope when we summon a Monad for Future:
  
  */
  
  			import cats.instances.future._ // for Monad
				import scala.concurrent._
				import scala.concurrent.duration._

	/*
					val fm = Monad[Future]
					// instance: cats.Monad[scala.concurrent.Future]
					// val fm = Monad[Future]
					//               ^

		Bringing the ExecutionContext into scope fixes the implicit resolution required
		to summon the instance:
  */
  
		import scala.concurrent.ExecutionContext.Implicits.global
		val fm = Monad[Future]

	/*
		The Monad instance uses the captured ExecutionContext for subsequent
		calls to pure and flatMap:
	*/
		
		val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))

		Await.result(future, 1.second)

	/*
		In addition to the above, Cats provides a host of new monads that we don’t
		have in the standard library. We’ll familiarise ourselves with some of these in
		a moment.

		4.2.3 Monad Syntax
		
		The syntax for monads comes from three places:
		
			• cats.syntax.flatMap provides syntax for flatMap;
			• cats.syntax.functor provides syntax for map;
			• cats.syntax.applicative provides syntax for pure.

		In practice it’s often easier to import everything in one go from
		cats.implicits. However, we’ll use the individual imports here for
		clarity.

		We can use pure to construct instances of a monad. We’ll often need to specify
		the type parameter to disambiguate the particular instance we want.
  */
  
		import cats.instances.option._ // for Monad
		import cats.instances.list._ // for Monad
		import cats.syntax.applicative._ // for pure

		1.pure[Option]

		1.pure[List]

	
	/*
		It’s difficult to demonstrate the flatMap and map methods directly on Scala
		monads like Option and List, because they define their own explicit versions
		of those methods. Instead we’ll write a generic function that performs a calcula
		tion on parameters that come wrapped in a monad of the user’s choice:
	*/
	
		import cats.Monad
		import cats.syntax.functor._ // for map
		import cats.syntax.flatMap._ // for flatMap
		import scala.language.higherKinds
	
		def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
			a.flatMap(x => b.map(y => x*x + y*y))

		import cats.instances.option._ // for Monad
		import cats.instances.list._ // for Monad

		sumSquare(Option(3), Option(4))

		sumSquare(List(1, 2, 3), List(4, 5))

  /*
		We can rewrite this code using for comprehensions. The compiler will “do the
		right thing” by rewriting our comprehension in terms of flatMap and map and
		inserting the correct implicit conversions to use our Monad:
	*/

		def sumSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
			for {
				x <- a
				y <- b
			} yield x*x + y*y

		sumSquare2(Option(3), Option(4))

		sumSquare2(List(1, 2, 3), List(4, 5))
  
  /*
		That’s more or less everything we need to know about the generalities of monads
		in Cats. Now let’s take a look at some useful monad instances that we
		haven’t seen in the Scala standard library.
  */
}
// package book.ch7.foldable.traverse

// oject c_traverse {
  /*
		7.2 Traverse
		foldLeft and foldRight are flexible iteration methods but they require us
		to do a lot of work to define accumulators and combinator functions. The
		Traverse type class is a higher level tool that leverages Applicatives to
		provide a more convenient, more lawful, pa􀂂ern for iteration.

		7.2.1 Traversing with Futures
		We can demonstrate Traverse using the Future.traverse and Future.
		sequence methods in the Scala standard library. These methods provide
		Future-specific implementations of the traverse pattern. As an example,
		suppose we have a list of server hostnames and a method to poll a host for its
		uptime:
	*/

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com")                             


  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration
                                                  

  /*
		Now, suppose we want to poll all of the hosts and collect all of their
		uptimes. We can’t simply map over hostnames because the result—a
		List[Future[Int]]—would contain more than one Future. We need to
		reduce the results to a single Future to get something we can block on. Let’s
		start by doing this manually using a fold:
	*/

  val allUptimes: Future[List[Int]] =

    hostnames.foldLeft(Future(List.empty[Int])) {
      (accum, host) =>
        val uptime = getUptime(host)
        for {
          accum <- accum
          uptime <- uptime
        } yield accum :+ uptime
    }                                             

  Await.result(allUptimes, 1.second)              

  /*
		Intuitively, we iterate over hostnames, call func for each item, and combine
		the results into a list. This sounds simple, but the code is fairly unwieldy because
		of the need to create and combine Futures at every iteration. We can
		improve on things greatly using Future.traverse, which is tailor-made for
		this pattern:
	*/

  val allUptimes2: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)         

  Await.result(allUptimes2, 1.second)             

  /*
		This is much clearer and more concise—let’s see how it works. If we ignore
		distractions like CanBuildFrom and ExecutionContext, the implementation
		of Future.traverse in the standard library looks like this:

	  def traverse[A, B](values: List[A])(func: A => Future[B]): Future[List[B]] =
	    values.foldLeft(Future(List.empty[A])) { (accum, host) =>
	      val item = func(host)
	      for {
	        accum <- accum
	        item <- item
	      } yield accum :+ item
	    }

	*/

  /*
		This is essentially the same as our example code above. Future.traverse is
		abstracting away the pain of folding and defining accumulators and combina-
		tion functions. It gives us a clean high-level interface to do what we want:
			• start with a List[A];
			• provide a function A => Future[B];
			• end up with a Future[List[B]].
		The standard library also provides another method, Future.sequence, that
		assumes we’re starting with a List[Future[B]] and don’t need to provide
		an identity function:

			object Future {
				def sequence[B](futures: List[Future[B]]): Future[List[B]] =
					traverse(futures)(identity)
					// etc...
				}


		In this case the intuitive understanding is even simpler:
			• start with a List[Future[A]];
			• end up with a Future[List[A]].

		Future.traverse and Future.sequence solve a very specific problem:
		they allow us to iterate over a sequence of Futures and accumulate a result.
		The simplified examples above only work with Lists, but the real Future.
		traverse and Future.sequence work with any standard Scala collec-
		tion.

		Cats’ Traverse type class generalises these patterns to work with any type
		of Applicative: Future, Option, Validated, and so on. We’ll approach
		Traverse in the next sections in two steps: first we’ll generalise over the
		Applicative, then we’ll generalise over the sequence type. We’ll end up
		with an extremely valuable tool that trivialises many operations involving sequences
		and other data types.

		7.2.2 Traversing with Applicatives

		If we squint, we’ll see that we can rewrite traverse in terms of an Applicative.
		Our accumulator from the example above:
			Future(List.empty[Int])

		is equivalent to Applicative.pure:
*/

  import cats.Applicative
  import cats.instances.future._ // for Applicative
  import cats.syntax.applicative._ // for pure
  List.empty[Int].pure[Future]                    

  /*
		Our combinator, which used to be this:

		def oldCombine(
			accum : Future[List[Int]],
			host : String
		): Future[List[Int]] = {
			val uptime = getUptime(host)
			for {
				accum <- accum
				uptime <- uptime
			} yield accum :+ uptime
		}

	is now equivalent to Semigroupal.combine:
*/
  import cats.syntax.apply._ // for mapN

  // Combining accumulator and hostname using an Applicative:
  def newCombine(
    accum: Future[List[Int]],
    host:  String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)         

  /*
	By substituting these snippets back into the definition of traverse we can
	generalise it to to work with any Applicative:
*/
  import scala.language.higherKinds

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }                                             


  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)                  


  //	We can use listTraverse to re-implement our uptime example:

  val totalUptime = listTraverse(hostnames)(getUptime)
                                                  


  Await.result(totalUptime, 1.second)             

  /*
		or we can use it with with other Applicative data types as shown in the
		following exercises.

		7.2.2.1 Exercise: Traversing with Vectors
			What is the result of the following?
	*/
  import cats.instances.vector._ // for Applicative
  listSequence(List(Vector(1, 2), Vector(3, 4)))  


  /*
		What about a list of three parameters?
	*/
  listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
                                                  



  /*
		7.2.2.2 Exercise: Traversing with Options
		Here’s an example that uses Options:
	*/

  import cats.instances.option._ // for Applicative
  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)
                                                  

  /*
		What is the return type of this method? What does it produce for the following
		inputs?
	*/

  process(List(2, 4, 6))                          
  process(List(1, 2, 3))                          

  /*
		7.2.2.3 Exercise: Traversing with Validated
		Finally, here is an example that uses Validated:
	*/

  import cats.data.Validated
  import cats.instances.list._ // for Monoid
  type ErrorsOr[A] = Validated[List[String], A]
  def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }                                             


  /*
		What does this method produce for the following inputs?
	*/

  process2(List(2, 4, 6))                         

  process2(List(1, 2, 3))                         


  /*
		7.2.3 Traverse in Cats
		Our listTraverse and listSequence methods work with any type of Applicative,
		but they only work with one type of sequence: List. We can
		generalise over different sequence types using a type class, which brings us
		to Cats’ Traverse. Here’s the abbreviated definition:

			// package cats

			trait Traverse[F[_]] {
				def traverse[G[_]: Applicative, A, B]
					(inputs: F[A])(func: A => G[B]): G[F[B]]

				def sequence[G[_]: Applicative, B]
					(inputs: F[G[B]]): G[F[B]] =
						traverse(inputs)(identity)
			}

		Cats provides instances of Traverse for List, Vector, Stream, Option, Either,
		and a variety of other types. We can summon instances as usual using
		Traverse.apply and use the traverse and sequence methods as described
		in the previous section:
	*/

  import cats.Traverse
  import cats.instances.future._ // for Applicative
  import cats.instances.list._ // for Traverse

  val totalUptime2: Future[List[Int]] =
    Traverse[List].traverse(hostnames)(getUptime) 

  Await.result(totalUptime2, 1.second)            

  val numbers = List(Future(1), Future(2), Future(3))
                                                  

  val numbers2: Future[List[Int]] =
    Traverse[List].sequence(numbers)              
  Await.result(numbers2, 1.second)                

  /*
		There are also syntax versions of the methods, imported via
		cats.syntax.traverse:
	*/

  import cats.syntax.traverse._ // for sequence and traverse
  Await.result(hostnames.traverse(getUptime), 1.second)
                                                  

  Await.result(numbers.sequence, 1.second)        

  /*
		As you can see, this is much more compact and readable than the foldLeft
		code we started with earlier this chapter!
		
		7.3 Summary
		In this chapter we were introduced to Foldable and Traverse, two type
		classes for iterating over sequences.

		Foldable abstracts the foldLeft and foldRight methods we know from
		collections in the standard library. It adds stack-safe implementations of these
		methods to a handful of extra data types, and defines a host of situationally
		useful additions. That said, Foldable doesn’t introduce much that we didn’t
		already know.

		The real power comes from Traverse, which abstracts and generalises the
		traverse and sequence methods we know from Future. Using these methods
		we can turn an F[G[A]] into a G[F[A]] for any F with an instance of Traverse
		and any G with an instance of Applicative. In terms of the reduction
		we get in lines of code, Traverse is one of the most powerful patterns in this
		book. We can reduce folds of many lines down to a single foo.traverse.
		
		
		…and with that, we’ve finished all of the theory in this book. There’s plenty
		more to come, though, as we put everything we’ve learned into practice in a
		series of in-depth case studies in Part II!
  */

// }
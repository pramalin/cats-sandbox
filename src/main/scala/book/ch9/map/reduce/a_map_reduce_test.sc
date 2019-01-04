package book.ch9

object a_map_reduce_test {
  /*
		Chapter 9
		Case Study: Map-Reduce
		In this case study we’re going to implement a simple-but-powerful parallel
		processing framework using Monoids, Functors, and a host of other goodies.

		If you have used Hadoop or otherwise worked in “big data” you will have heard
		of MapReduce, which is a programming model for doing parallel data processing
		across clusters of machines (aka “nodes”). As the name suggests, the model
		is built around a map phase, which is the same map func􀦞on we know from
		Scala and the Functor type class, and a reduce phase, which we usually call
		fold in Scala. (In Hadoop there is also a shuffle phase that we will ignore here.)

		9.1 Parallelizing map and fold

		Recall the general signature for map is to apply a func􀦞on A => B to a F[A],
		returning a F[B]:

		map transforms each individual element in a sequence independently. We can
		easily parallelize map because there are no dependencies between the transforma
		􀦞ons applied to different elements (the type signature of the func􀦞on
		A => B shows us this, assuming we don’t use side-effects not reflected in the
		types).


		(Figure 9.1: Type chart: functor map)

		(Figure 9.2: Type chart: fold)

		What about fold? We can implement this step with an instance of Foldable.
		Not every functor also has an instance of foldable but we can implement a
		map-reduce system on top of any data type that has both of these type classes.
		Our reduction step becomes a foldLeft over the results of the distributed
		map.

		By distributing the reduce step we lose control over the order of traversal.
		Our overall reduction may not be entirely left-to-right—we may reduce left-
		to-right across several subsequences and then combine the results. To ensure
		correctness we need a reduction operation that is associative:

			reduce(a1, reduce(a2, a3)) == reduce(reduce(a1, a2), a3)

		If we have associativity, we can arbitrarily distribute work between our nodes
		provided the subsequences at every node stay in the same order as the initial
		dataset.

		Our fold operation requires us to seed the computation with an element of
		type B. Since fold may be split into an arbitrary number of parallel steps, the
		seed should not affect the result of the computation. This naturally requires
		the seed to be an identity element:

			reduce(seed, a1) == reduce(a1, seed) == a1

		In summary, our parallel fold will yield the correct results if:
			• we require the reducer function to be associative;
			• we seed the computation with the identity of this function.

		What does this pattern sound like? That’s right, we’ve come full circle back
		to Monoid, the first type class we discussed in this book. We are not the
		first to recognise the importance of monoids. The monoid design pattern for
		map-reduce jobs is at the core of recent big data systems such as Twitter’s
		Summingbird.

		In this project we’re going to implement a very simple single-machine mapreduce.
		We’ll start by implementing a method called foldMap to model the
		data-flow we need.

		9.2 Implementing foldMap

		We saw foldMap briefly back when we covered Foldable. It is one of the
		derived operations that sits on top of foldLeft and foldRight. However,
		rather than use Foldable, we will re-implement foldMap here ourselves as it
		will provide useful insight into the structure of map-reduce.

		Start by writing out the signature of foldMap. It should accept the following
		parameters:
			• a sequence of type Vector[A];
			• a function of type A => B, where there is a Monoid for B;

		You will have to add implicit parameters or context bounds to complete the
		type signature.
			See the solution
			*/
  /*
			import cats.Monoid
			/** Single-threaded map-reduce function.
			* Maps `func` over `values` and reduces using a `Monoid[B]`.
			*/
			def foldMap[A, B: Monoid](values: Vector[A])(func: A => B): B =
			???
*/

  /*
		Now implement the body of foldMap. Use the flow chart in Figure 9.3 as a
		guide to the steps required:

		(Figure 9.3: foldMap algorithm)

		1. start with a sequence of items of type A;
		2. map over the list to produce a sequence of items of type B;
		3. use the Monoid to reduce the items to a single B.
		Here’s some sample output for reference:
	*/

  import sandbox.usecases.mapreduce.MapReduce._
  import cats.instances.int._ // for Monoid

  foldMap(Vector(1, 2, 3))(identity)              //> res0: Int = 6

  import cats.instances.string._ // for Monoid

  // Mapping to a String uses the concatenation monoid:
  foldMap(Vector(1, 2, 3))(_.toString + "! ")     //> res1: String = "1! 2! 3! "

  // Mapping over a String to produce a String:
  foldMap("Hello world!".toVector)(_.toString.toUpperCase)
                                                  //> res2: String = HELLO WORLD!

  /*
		9.3 Parallelising foldMap

		Now we have a working single-threaded implementation of foldMap, let’s
		look at distributing work to run in parallel. We’ll use our single-threaded version
		of foldMap as a building block.

		We’ll write a multi-CPU implementation that simulates the way we would distribute
		work in a map-reduce cluster as shown in Figure 9.4:
			1. we start with an initial list of all the data we need to process;
			2. we divide the data into batches, sending one batch to each CPU;
			3. the CPUs run a batch-level map phase in parallel;
			4. the CPUs run a batch-level reduce phase in parallel, producing a local
			result for each batch;
			5. we reduce the results for each batch to a single final result.

		Scala provides some simple tools to distribute work amongst threads. We
		could use the parallel collections library to implement a solution, but let’s challenge
		ourselves by diving a bit deeper and implementing the algorithm ourselves
		using Futures.

		9.3.1 Futures, Thread Pools, and ExecutionContexts

		We already know a fair amount about the monadic nature of Futures. Let’s
		take a moment for a quick recap, and to describe how Scala futures are scheduled
		behind the scenes.

		Futures run on a thread pool, determined by an implicit ExecutionContext
		parameter. Whenever we create a Future, whether through a call to

		(Figure 9.4: parallelFoldMap algorithm)

	Future.apply or some other combinator, we must have an implicit ExecutionContext
		in scope:
	*/
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  val future1 = Future {
    (1 to 100).toList.foldLeft(0)(_ + _)
  }                                               //> future1  : scala.concurrent.Future[Int] = Future(<not completed>)

  val future2 = Future {
    (100 to 200).toList.foldLeft(0)(_ + _)
  }                                               //> future2  : scala.concurrent.Future[Int] = Future(<not completed>)

  /*
		In this example we’ve imported a ExecutionContext.Implicits.global.
		This default context allocates a thread pool with one thread per CPU in our
		machine. When we create a Future the ExecutionContext schedules it for
		execution. If there is a free thread in the pool, the Future starts executing immediately.
		Most modern machines have at least two CPUs, so in our example
		it is likely that future1 and future2 will execute in parellel.

		Some combinators create new Futures that schedule work based on the results
		of other Futures. The map and flatMap methods, for example, schedule
		computations that run as soon as their input values are computed and a CPU
		is available:
	*/

  val future3 = future1.map(_.toString)           //> future3  : scala.concurrent.Future[String] = Future(<not completed>)

  val future4 = for {
    a <- future1
    b <- future2
  } yield a + b                                   //> future4  : scala.concurrent.Future[Int] = Future(<not completed>)

  /*
		As we saw in Section 7.2, we can convert a List[Future[A]] to a Future[
		List[A]] using Future.sequence:
	*/

  Future.sequence(List(Future(1), Future(2), Future(3)))
                                                  //> res3: scala.concurrent.Future[List[Int]] = Future(Success(List(1, 2, 3)))

  // or an instance of Traverse:
  import cats.instances.future._ // for Applicative
  import cats.instances.list._ // for Traverse
  import cats.syntax.traverse._ // for sequence
  List(Future(1), Future(2), Future(3)).sequence  //> res4: scala.concurrent.Future[List[Int]] = Future(Success(List(1, 2, 3)))

  /*
		An ExecutionContext is required in either case. Finally, we can use
		Await.result to block on a Future until a result is available:
	*/

  import scala.concurrent._
  import scala.concurrent.duration._
  Await.result(Future(1), 1.second) // wait for the result
                                                  //> res5: Int = 1

  /*
		There are also Monad and Monoid implementations for Future available from
		cats.instances.future:
	*/

  import cats.{ Monad, Monoid }
  import cats.instances.int._ // for Monoid
  import cats.instances.future._ // for Monad and Monoid
  Monad[Future].pure(42)                          //> res6: scala.concurrent.Future[Int] = Future(Success(42))
  Monoid[Future[Int]].combine(Future(1), Future(2))
                                                  //> res7: scala.concurrent.Future[Int] = Future(<not completed>)

  /*
		9.3.2 Dividing Work

		Now we’ve refreshed our memory of Futures, let’s look at how we can divide
		work into batches. We can query the number of available CPUs on our
		machine using an API call from the Java standard library:
	*/
  Runtime.getRuntime.availableProcessors          //> res8: Int = 4

  /*
		We can partition a sequence (actually anything that implements Vector) using
		the grouped method. We’ll use this to split off batches of work for each CPU:
	*/

  (1 to 10).toList.grouped(3).toList              //> res9: List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), L
                                                  //| ist(10))

  /*
		9.3.3 Implemen􀦞ng parallelFoldMap

		Implement a parallel version of foldMap called parallelFoldMap. Here is
		the type signature:

  		def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = ???
	*/

  /*
		Use the techniques described above to split the work into batches, one batch
		per CPU. Process each batch in a parallel thread. Refer back to Figure 9.4 if
		you need to review the overall algorithm.
		For bonus points, process the batches for each CPU using your implementa-
		tion of foldMap from above.
		See the solution
	*/

  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)
                                                  //> result  : scala.concurrent.Future[Int] = Future(<not completed>)
  Await.result(result, 1.second)                  //> res10: Int = 1784293664

  val result1: Future[Int] =
    parallelFoldMap1((1 to 1000000).toVector)(identity)
                                                  //> result1  : scala.concurrent.Future[Int] = Future(<not completed>)
  Await.result(result1, 1.second)                 //> res11: Int = 1784293664

  /*
		9.3.4 parallelFoldMap with more Cats
		Although we implemented foldMap ourselves above, the method is also available
		as part of the Foldable type class we discussed in Section 7.1.
		Reimplement parallelFoldMap using Cats’ Foldable and Traverseable
		type classes.
		See the solution
	*/
  val future: Future[Int] =
    parallelFoldMap2((1 to 1000).toVector)(_ * 1000)
                                                  //> future  : scala.concurrent.Future[Int] = Future(<not completed>)
  Await.result(future, 1.second)                  //> res12: Int = 500500000

  /*
		9.4 Summary
		In this case study we implemented a system that imitates map-reduce as performed
		on a cluster. Our algorithm followed three steps:

			1. batch the data and send one batch to each “node”;
			2. perform a local map-reduce on each batch;
			3. combine the results using monoid addition.

		Our toy system emulates the batching behaviour of real-world map-reduce
		systems such as Hadoop. However, in reality we are running all of our work
		on a single machine where communcation between nodes is negligable. We
		don’t actually need to batch data to gain efficient parallel processing of a list.
		We can simply map using a Functor and reduce using a Monoid.

		Regardless of the batching strategy, mapping and reducing with Monoids is a
		powerful and general framework that isn’t limited to simple tasks like addition
		and string concatenation. Most of the tasks data scientists perform in their
		day-to-day analyses can be cast as monoids. There are monoids for all the
		following:
			• approximate sets such as the Bloom filter;
			• set cardinality estimators, such as the HyperLogLog algorithm;
			• vectors and vector operations like stochastic gradient descent;
			• quantile estimators such as the t-digest
			to name but a few.
	*/
	
}
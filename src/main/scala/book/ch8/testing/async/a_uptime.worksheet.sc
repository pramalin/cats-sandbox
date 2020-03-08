// package book.ch8

// oject a_uptime {
  println("Welcome to the Scala worksheet")       

/*
	Chapter 8
	
	Case Study: Testing Asynchronous Code
	
	We’ll start with a straightforward case study: how to simplify unit tests for
	asynchronous code by making them synchronous.
	
	Let’s return to the example from Chapter 7 where we’re measuring the uptime
	on a set of servers. We’ll flesh out the code into a more complete structure.
	There will be two components. The first is an UptimeClient that polls remote
	servers for their uptime:
	
				import scala.concurrent.Future
					trait UptimeClient {
					def getUptime(hostname: String): Future[Int]
				}
	
	We’ll also have an UptimeService that maintains a list of servers and allows
	the user to poll them for their total uptime:
	
				import cats.instances.future._ // for Applicative
				import cats.instances.list._ // for Traverse
				import cats.syntax.traverse._ // for traverse
				import scala.concurrent.ExecutionContext.Implicits.global
	
				class UptimeService(client: UptimeClient) {
					def getTotalUptime(hostnames: List[String]): Future[Int] =
						hostnames.traverse(client.getUptime).map(_.sum)
				}
	
	We’ve modelled UptimeClient as a trait because we’re going to want to stub
	it out in unit tests. For example, we can write a test client that allows us to
	provide dummy data rather than calling out to actual servers:
	
				class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient {
					def getUptime(hostname: String): Future[Int] =
						Future.successful(hosts.getOrElse(hostname, 0))
				}
	
	Now, suppose we’re writing unit tests for UptimeService. We want to test
	its ability to sum values, regardless of where it is getting them from. Here’s an
	example:
	
				def testTotalUptime() = {
					val hosts = Map("host1" -> 10, "host2" -> 6)
					val client = new TestUptimeClient(hosts)
					val service = new UptimeService(client)
					val actual = service.getTotalUptime(hosts.keys.toList)
					val expected = hosts.values.sum
					assert(actual == expected)
				}
				// <console>:31: warning: scala.concurrent.Future[Int] and Int are
				unrelated: they will most likely never compare equal
				// assert(actual == expected)
				// ^
				// error: No warnings can be incurred under -Xfatal-warnings.
	
	The code doesn’t compile because we’ve made a classic error¹. We forgot
	that our application code is asynchronous. Our actual result is of type Future[
	Int] and out expected result is of type Int. We can’t compare them
	directly!
	
	----------------------
	¹Technically this is a warning not an error. It has been promoted to an error in our case
	because we’re using the -Xfatal-warnings flag on scalac.
	
	There are a couple of ways to solve this problem. We could alter our test
	code to accommodate the asynchronousness. However, there is another alterna
	tive. Let’s make our service code synchronous so our test works without
	modification!
	
	8.1 Abstracting over Type Constructors
	
	We need to implement two versions of UptimeClient: an asynchronous one
	for use in production and a synchronous one for use in our unit tests:
	
				trait RealUptimeClient extends UptimeClient {
					def getUptime(hostname: String): Future[Int]
				}
	
				trait TestUptimeClient extends UptimeClient {
					def getUptime(hostname: String): Int
				}
	The question is: what result type should we give to the abstract method in
	UptimeClient? We need to abstract over Future[Int] and Int:
	
				trait UptimeClient {
					def getUptime(hostname: String): ???
				}
	
	At first this may seem difficult. We want to retain the Int part from each
	type but “throw away” the Future part in the test code. Fortunately, Cats
	provides a solution in terms of the iden􀢼ty type, Id, that we discussed way
	back in Section 4.3. Id allows us to “wrap” types in a type constructor without
	changing their meaning:
	
				// package cats
				type Id[A] = A
	
	Id allows us to abstract over the return types in UptimeClient. Implement
	this now:
	
		• write a trait definition for UptimeClient that accepts a type constructor
		F[_] as a parameter;
		• extend it with two traits, RealUptimeClient and TestUptimeClient,
		that bind F to Future and Id respectively;
		• write out the method signature for getUptime in each case to verify
		that it compiles.
	
			----------------------- solution ------------------------------
			H.1 Abstracting over Type Constructors
			Here’s the implementation:
*/
			import scala.language.higherKinds
			import cats.Id
			import scala.concurrent.{Future, Await}
		
			trait UptimeClient[F[_]] {
				def getUptime(hostname: String): F[Int]
			}
		
			trait RealUptimeClient extends UptimeClient[Future] {
				def getUptime(hostname: String): Future[Int]
			}
			
			trait TestUptimeClient1 extends UptimeClient[Id] {
				def getUptime(hostname: String): Id[Int]
			}
			
			/*
			Note that, because Id[A] is just a simple alias for A, we don’t need to refer to
			the type in TestUptimeClient as Id[Int]—we can simply write Int instead:
			*/
			
			trait TestUptimeClient2 extends UptimeClient[Id] {
				def getUptime(hostname: String): Int
			}
	
			/*
			Of course, technically speaking we don’t need to redeclare getUptime in
			RealUptimeClient or TestUptimeClient. However, writing everything
			out helps illustrate the technique.

		  ---------------------------------------------------------------
	
	You should now be able to flesh your definition of TestUptimeClient out
	into a full class based on a Map[String, Int] as before.

			----------------------- solution ------------------------------
			H.2 Abstracting over Type Constructors Part 2
			The final code is similar to our original implementation of TestUptimeClient,
			except we no longer need the call to Future.successful:
	*/
			class TestUptimeClient3(hosts: Map[String, Int])
				extends UptimeClient[Id] {
				def getUptime(hostname: String): Int =
					hosts.getOrElse(hostname, 0)
			}
	/*
		  ---------------------------------------------------------------

	8.2 Abstracting over Monads
	Let’s turn our attention to UptimeService. We need to rewrite it to abstract
	over the two types of UptimeClient. We’ll do this in two stages: first we’ll
	rewrite the class and method signatures, then the method bodies. Starting
	with the method signatures:
		• comment out the body of getTotalUptime (replace it with ??? to
		make everything compile);
		• add a type parameter F[_] to UptimeService and pass it on to UptimeClient.

			----------------------- solution ------------------------------
		H.3 Abstracting over Monads
		
		The code should look like this:
	*/
			class UptimeService[F[_]](client: UptimeClient[F]) {
				def getTotalUptime(hostnames: List[String]): F[Int] =
					???
					// hostnames.traverse(client.getUptime).map(_.sum)
			}
	/*
		  ---------------------------------------------------------------

	Now uncomment the body of getTotalUptime. You should get a compilation
	error similar to the following:
	
				// <console>:28: error: could not find implicit value for
				// evidence parameter of type cats.Applicative[F]
				// hostnames.traverse(client.getUptime).map(_.sum)
				// ^

	The problem here is that traverse only works on sequences of values
	that have an Applicative. In our original code we were traversing a
	List[Future[Int]]. There is an applicative for Future so that was fine.
	In this version we are traversing a List[F[Int]]. We need to prove to the
	compiler that F has an Applicative. Do this by adding an implicit constructor
	parameter to UptimeService.

			----------------------- solution ------------------------------
			H.4 Abstrac􀦞ng over Monads Part 2
			We can write this as an implicit parameter:
	*/
			import cats.Applicative
			import cats.syntax.functor._ // for map
			import cats.instances.list._ // for Traverse
			import cats.syntax.traverse._ // for traverse
			
			class UptimeService1[F[_]](client: UptimeClient[F])
					(implicit a: Applicative[F]) {
					
				def getTotalUptime(hostnames: List[String]): F[Int] =
					hostnames.traverse(client.getUptime).map(_.sum)
			}
	/*
			or more tersely as a context bound:
	*/
			class UptimeService2[F[_]: Applicative]
					(client: UptimeClient[F]) {
					
				def getTotalUptime(hostnames: List[String]): F[Int] =
					hostnames.traverse(client.getUptime).map(_.sum)
			}
	
	/*
			Note that we need to import cats.syntax.functor as well as
			cats.Applicative. This is because we’re switching from using future.map
			to the Cats’ generic extension method that requires an implicit Functor parameter.
		  ---------------------------------------------------------------

	Finally, let’s turn our attention to our unit tests. Our test code now works as
	intended without any modification. We create an instance of TestUptime-
	Client and wrap it in an UptimeService. This effectively binds F to Id, allowing
	the rest of the code to operate synchronously without worrying about
	monads or applicatives:

				def testTotalUptime() = {
					val hosts = Map("host1" -> 10, "host2" -> 6)
					val client = new TestUptimeClient(hosts)
					val service = new UptimeService(client)
					val actual = service.getTotalUptime(hosts.keys.toList)
					val expected = hosts.values.sum
					assert(actual == expected)
				}
				
				testTotalUptime()

	8.3 Summary
	
	This case study provides an example of how Cats can help us abstract over
	different computational scenarios. We used the Applicative type class to
	abstract over asynchronous and synchronous code. Leaning on a functional
	abstraction allows us to specify the sequence of computations we want to
	perform without worrying about the details of the implementation.
	
	Back in Figure 6.1, we showed a “stack” of computational type classes that
	are meant for exactly this kind of abstraction. Type classes like Functor, Applicative,
	Monad, and Traverse provide abstract implementations of patterns
	such as mapping, zipping, sequencing, and iteration. The mathematical
	laws on those types ensure that they work together with a consistent set of
	semantics.
	
	We used Applicative in this case study because it was the least powerful
	type class that did what we needed. If we had required flatMap, we could
	have swapped out Applicative for Monad. If we had needed to abstract over
	different sequence types, we could have used Traverse. There are also type
	classes like ApplicativeError and MonadError that help model failures as
	well as successful computations.
	
	Let’s move on now to a more complex case study where type classes will help
	us produce something more interesting: a map-reduce-style framework for
	parallel processing.
*/
// }
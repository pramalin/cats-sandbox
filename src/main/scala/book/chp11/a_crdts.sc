package book.chp11

object a_crdts {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet


	/*
		Chapter 11
		
		Case Study: CRDTs
		
		In this case study we will explore Commutative Replicated Data Types (CRDTs),
		a family of data structures that can be used to reconcile eventually consistent
		data.
		
		We’ll start by describing the utility and difficulty of eventually consistent systems,
		then show how we can use monoids and their extensions to solve the
		issues that arise. Finally, we will model the solutions in Scala.
		
		Our goal here is to focus on the implementation in Scala of a particular type
		of CRDT. We’re not aiming at a comprehensive survey of all CRDTs. CRDTs
		are a fast-moving field and we advise you to read the literature to learn about
		more.
		
		11.1 Eventual Consistency
		
		As soon as a system scales beyond a single machine we have to make a fundamental
		choice about how we manage data.
		
		One approach is to build a system that is consistent, meaning that all machines
		have the same view of data. For example, if a user changes their password
		then all machines that store a copy of that password must accept the change
		before we consider the operation to have completed successfully.
		
		Consistent systems are easy to work with but they have their disadvantages.
		They tend to have high latency because a single change can result in many
		messages being sent between machines. They also tend to have relatively low
		uptime because outages can cut communications between machines creating
		a network partition. When there is a network partition, a consistent system
		may refuse further updates to prevent inconsistencies across machines.
		
		An alternative approach is an eventually consistent system. This means that
		at any particular point in time machines are allowed to have differing views
		of data. However, if all machines can communicate and there are no further
		updates they will eventually all have the same view of data.
		
		Eventually consistent systems require less communication between machines
		so latency can be lower. A partitioned machine can still accept updates and
		reconcile its changes when the network is fixed, so systems can also can have
		better uptime.
		
		The big question is: how do we do this reconciliation between machines?
		CRDTs provide one approach to the problem.
		
		11.2 The GCounter
		
		Let’s look at one particular CRDT implementation. Then we’ll attempt to generalise
		properties to see if we can find a general pattern.
		
		The data structure we will look at is called a GCounter. It is a distributed
		increment-only counter that can be used, for example, to count the number
		of visitors to a web site where requests are served by many web servers.

		11.2.1 Simple Counters

		To see why a straightforward counter won’t work, imagine we have two
		servers storing a simple count of visitors. Let’s call the machines A and B.
		Each machine is storing an integer counter and the counters all start at zero
		as shown in Figure 11.1.

                     Machine A         Machine B
                     +-------+         +-------+
                     |       |         |       |
                     |   0   |         |   0   |
                     |       |         |       |
                     +-------+         +-------+
					(Figure 11.1: Simple counters: initial state)



                     Machine A         Machine B
                     +-------+         +-------+
  Incoming requests  |       |         |       |  Incoming requests
              +------>   3   |\      / |   2   <-----------+
                     |       | \    /  |       |
                     +-------+  \  /   +-------+
                              Add\/counters
                                 /\
                     Machine A  /  \   Machine B
                     +-------+ /    \  +-------+
                     |       |v      v |       |
                     |   5   |         |   5   |
                     |       |         |       |
                     +-------+         +-------+
					(Figure 11.2: Simple counters: first round of requests and reconciliation)

		Now imagine we receive some web traffic. Our load balancer distributes five
		incoming requests to A and B, A serving three visitors and B two. The machines
		have inconsistent views of the system state that they need to reconcile
		to achieve consistency. One reconciliation strategy with simple counters is to
		exchange counts and add them as shown in Figure 11.2.
		
		So far so good, but things will start to fall apart shortly. Suppose A serves
		a single visitor, which means we’ve seen six visitors in total. The machines
		attempt to reconcile state again using addition leading to the answer shown
		in Figure 11.3.
		
		This is clearly wrong! The problem is that simple counters don’t give us enough
		information about the history of interactions between the machines. Fortunately
		we don’t need to store the complete history to get the correct answer—
		just a summary of it. Let’s look at the GCounter see how it solves this problem.

                     Machine A         Machine B
                     +-------+         +-------+
  Incoming requests  |       |         |       |
              +------>   6   |\      / |   5   |
                     |       | \    /  |       |
                     +-------+  \  /   +-------+
                              Add\/counters
                                 /\
                     Machine A  /  \   Machine B
                     +-------+ /    \  +-------+
                     |       |v      v |       |
                     |  11   |         |  11   |    X Incorrect result!
                     |       |         |       |
                     +-------+         +-------+

					Figure 11.3: Simple counters: second round of requests and (incorrect) reconcilia
					tion
					
                     Machine A         Machine B
                     +-------+         +-------+
                     | A:0   |         | A:0   |
                     | B:0   |         | B:0   |
                     |       |         |       |
                     +-------+         +-------+
					
					(Figure 11.4: GCounter: initial state)

		11.2.2 GCounters

		The first clever idea in the GCounter is to have each machine storing a separate
		counter for every machine it knows about (including itself). In the previous
		example we had two machines, A and B. In this situation both machines would
		store a counter for A and a counter for B as shown in Figure 11.4.

		The rule with GCounters is that a given machine is only allowed to increment
		its own counter. If A serves three visitors and B serves two visitors the counters
		look as shown in Figure 11.5.

		When two machines reconcile their counters the rule is to take the largest
		value stored for each machine. In our example, the result of the first merge
		will be as shown in Figure 11.6.

		Subsequent incoming web requests are handled using the increment-owncounter
		rule and subsequent merges are handled using the take-maximumvalue
		rule, producing the same correct values for each machine as shown in
		Figure 11.7.

                     Machine A         Machine B
                     +-------+         +-------+
  Incoming requests  | A:3   |         | A:0   |  Incoming requests
              +------> B:0   |         | B:2   <------+
                     |       |         |       |
                     +-------+         +-------+

 					(Figure 11.5: GCounter: first round of web requests)

                     Machine A         Machine B
                     +-------+         +-------+
  Incoming requests  | A:3   |         | A:0   |  Incoming requests
              +------> B:0   |\      / | B:2   <-----------+
                     |       | \    /  |       |
                     +-------+  \  /   +-------+
                           Merge,\/take max
                                 /\
                     Machine A  /  \   Machine B
                     +-------+ /    \  +-------+
                     | A:3   |v      v | A:3   |
                     | B:2   |         | B:2   |
                     |       |         |       |
                     +-------+         +-------+

					(Figure 11.6: GCounter: first reconciliation)


		GCounters allow each machine to keep an accurate account of the state of
		the whole system without storing the complete history of interactions. If a
		machine wants to calculate the total traffic for the whole web site, it sums up
		all the per-machine counters. The result is accurate or near-accurate depending
		on how recently we performed a reconciliation. Eventually, regardless of
		network outages, the system will always converge on a consistent state.

		11.2.3 Exercise: GCounter Implementation

		We can implement a GCounter with the following interface, where we represent
		machine IDs as Strings.

			final case class GCounter(counters: Map[String, Int]) {
				def increment(machine: String, amount: Int) =
					???
			
				def merge(that: GCounter): GCounter =
					???
			
				def total: Int =
					???
			}
		

                     Machine A         Machine B
                     +-------+         +-------+
  Incoming requests  | A:4   |         | A:3   |
              +------> B:2   |\      / | B:2   |
                     |       | \    /  |       |
                     +-------+  \  /   +-------+
                           Merge,\/take max
                                 /\
                     Machine A  /  \   Machine B
                     +-------+ /    \  +-------+
                     | A:4   |v      v | A:4   |
                     | B:2   |         | B:2   | _/ Correct result!
                     |       |         |       |
                     +-------+         +-------+
						Figure 11.7: GCounter: second reconciliation
	
		
		Finish the implementation!
		See the solution
			
	  ----------------------- solution ------------------------------
			K.1 GCounter Implementation
			
			Hopefully the description above was clear enough that you can get to an implementa
			tion like the one below.
			
			final case class GCounter(counters: Map[String, Int]) {
					def increment(machine: String, amount: Int) = {
						val value = amount + counters.getOrElse(machine, 0)
						GCounter(counters + (machine -> value))
					}
				
				def merge(that: GCounter): GCounter =
					GCounter(that.counters ++ this.counters.map {
						case (k, v) =>
							k -> (v max that.counters.getOrElse(k, 0))
					})
				
				def total: Int =
					counters.values.sum
			}

    ---------------------------------------------------------------
			
		11.3 Generalisation

		We’ve now created a distributed, eventually consistent, increment-only
		counter. This is a useful achievement but we don’t want to stop here. In this
		section we will attempt to abstract the operations in the GCounter so it will
		work with more data types than just natural numbers.

		The GCounter uses the following operations on natural numbers:

			• addition (in increment and total);
			• maximum (in merge);
			• and the identity element 0 (in increment and merge).

		You can probably guess that there’s a monoid in here somewhere, but let’s look
		in more detail at the properties we’re relying on.

		As a refresher, in Chapter 2 we saw that monoids must satisfy two laws. The
		binary operation + must be associative:

			(a + b) + c == a + (b + c)
	
		and the empty element must be an identity:
	
			0 + a == a + 0 == a
		
		We need an identity in increment to initialise the counter. We also rely on associa
		tivity to ensure the specific sequence of merges gives the correct value.
	
		In total we implicitly rely on associativity and commutativity to ensure we
		get the correct value no matter what arbitrary order we choose to sum the
		per-machine counters. We also implicitly assume an identity, which allows us
		to skip machines for which we do not store a counter.
	
		The properties of merge are a bit more interesting. We rely on commutativity
		to ensure that machine A merging with machine B yields the same result as
		machine B merging with machine A. We need associativity to ensure we obtain
		the correct result when three or more machines are merging data. We
		need an identity element to initialise empty counters. Finally, we need an addi
		tional property, called idempotency, to ensure that if two machines hold the
		same data in a per-machine counter, merging data will not lead to an incorrect
		result. Idempotent operations are ones that return the same result again and
		again if they are executed multiple times. Formally, a binary operation max is
		idempotent if the following relationship holds:
	
			a max a = a
		
		Written more compactly, we have:
		
		------------------------------------------------------------------
		 Method        Identity   Commutative   Associative   Idempotent
		------------------------------------------------------------------
		 increment         Y          N              Y            N
		 merge             Y          Y              Y            Y
		 total             Y          Y              Y            N
		------------------------------------------------------------------
		
		From this we can see that
			• increment requires a monoid;
			• total requires a commutative monoid; and
			• merge required an idempotent commutative monoid, also called a
				bounded semilattice.
		
		Since increment and total both use the same binary operation (addition) it’s
		usual to require the same commutative monoid for both.
		
		This investigation demonstrates the powers of thinking about properties or
		laws of abstractions. Now we have identified these properties we can subs
		titute the natural numbers used in our GCounter with any data type with
		operations satisfying these properties. A simple example is a set, with the binary
		operation being union and the identity element the empty set. With this
		simple substitution of Int for Set[A] we can create a GSet type.
		
		11.3.1 Implementation
		
		Let’s implement this generalisation in code. Remember increment and total
		require a commutative monoid and merge requires a bounded semilattice (or
		idempotent commutative monoid).
		
		Cats provides a Monoid, but no commutative monoid or bounded semilattice
		type class. (A closely related library called Spire provides both these abstractions.)
		For simplicity of implementation we’ll use Monoid when we really
		mean a commutative monoid, and require the programmer to ensure the implementa
		tion is commutative. We’ll implement our own BoundedSemiLattice
		type class.
		
			import cats.Monoid

			trait BoundedSemiLattice[A] extends Monoid[A] {
				def combine(a1: A, a2: A): A
			
				def empty: A
			}
		
		In the implementation above, BoundedSemiLattice[A] extends Monoid[A]
		because a bounded semilattice is a monoid (a commutative idempotent one,
		to be exact).
		
		11.3.2 Exercise: BoundedSemiLattice Instances
		
		Implement BoundedSemiLattice type class instances for Ints and for Sets.
		The instance for Int will technically only hold for non-negative numbers, but
		you don’t need to model non-negativity explicitly in the types.
		
		See the solution

	  ----------------------- solution ------------------------------
			K.2 BoundedSemiLattice Instances

			It’s common to place the instances in the companion object of
			BoundedSemiLattice so they are in the implicit scope without importing
			them.

			Implementing the instance for Set provides good practice with implicit methods.

				trait BoundedSemiLattice[A] extends Monoid[A] {
					def combine(a1: A, a2: A): A
					def empty: A
				}

				object BoundedSemiLattice {
					implicit val intInstance: BoundedSemiLattice[Int] =
						new BoundedSemiLattice[Int] {
							def combine(a1: Int, a2: Int): Int =
								a1 max a2
								
							val empty: Int =
								0
						}
						
					implicit def setInstance[A]: BoundedSemiLattice[Set[A]] =
						new BoundedSemiLattice[Set[A]]{
							def combine(a1: Set[A], a2: Set[A]): Set[A] =
								a1 union a2
					
							val empty: Set[A] =
								Set.empty[A]
						}
				}
    ---------------------------------------------------------------
		
		11.3.3 Exercise: Generic GCounter

		Using Monoid and BoundedSemiLattice, generalise GCounter.

		When you implement this, look for opportunities to use methods and syntax
		on Monoid to simplify your implementation. This is a good example of
		how type class abstractions work at multiple levels in our code. We’re using
		monoids to design a large component—our CRDTs—but they are also useful in
		the small, simplifying our code and making it shorter and clearer.

		See the solution

	  ----------------------- solution ------------------------------
			K.3 Generic GCounter

			Here’s a working implementation. Note the use of |+| in the definition of
			merge, which significantly simplifies the process of merging and maximising
			counters:

			import cats.instances.list._ // for Monoid
			import cats.instances.map._ // for Monoid
			import cats.syntax.semigroup._ // for |+|
			import cats.syntax.foldable._ // for combineAll

			final case class GCounter[A](counters: Map[String,A]) {
				def increment(machine: String, amount: A)
							(implicit m: Monoid[A]) = {
					val value = amount |+| counters.getOrElse(machine, m.empty)
					GCounter(counters + (machine -> value))
				}
		
				def merge(that: GCounter[A])
						(implicit b: BoundedSemiLattice[A]): GCounter[A] =
					GCounter(this.counters |+| that.counters)
				
				def total(implicit m: Monoid[A]): A =
					this.counters.values.toList.combineAll
			}
    ---------------------------------------------------------------

		11.4 Abstracting GCounter to a Type Class

		We’ve created a generic GCounter that works with any value that has instances
		of BoundedSemiLattice and (commutative) Monoid. However we’re
		still tied to a particular representation of the map from machine IDs to values.
		There is no need to have this restriction, and indeed it can be useful to abstract
		away from it. There are many key-value stores that we want to work
		with, from a simple Map to a relational database.

		If we define a GCounter type class we can abstract over different concrete
		implementations. This allows us to, for example, seamlessly substitute an inmemory
		store for a persistent store when we want to change performance
		and durability tradeoffs.

		There are a number of ways we can implement this. One approach
		is to define a GCounter type class with dependencies on Monoid and
		BoundedSemiLattice. We define this as a type class that takes a type constructor
		with two type parameters represent the key and value types of the
		map abstraction.

			trait GCounter[F[_,_],K, V] {
				def increment(f: F[K, V])(k: K, v: V)
							(implicit m: Monoid[V]): F[K, V]
				
				def merge(f1: F[K, V], f2: F[K, V])
							(implicit b: BoundedSemiLattice[V]): F[K, V]
				
				def total(f: F[K, V])
							(implicit m: Monoid[V]): V
				}
				
				object GCounter {
					def apply[F[_,_], K, V]
								(implicit counter: GCounter[F, K, V]) =
					counter
			}
			
		Try defining an instance of this type class for Map. You should be able to reuse
		your code from the case class version of GCounter with some minor modifica
		tions.
		
	  ----------------------- solution ------------------------------
			K.4 Abstracting GCounter to a Type Class
			Here’s the complete code for the instance. Write this definition in the companion
			object for GCounter to place it in glocal implicit scope:

			import cats.instances.list._ // for Monoid
			import cats.instances.map._ // for Monoid
			import cats.syntax.semigroup._ // for |+|
			import cats.syntax.foldable._ // for combineAll

			implicit def mapInstance[K, V]: GCounter[Map, K, V] =
				new GCounter[Map, K, V] {
					def increment(map: Map[K, V])(key: K, value: V)
						(implicit m: Monoid[V]): Map[K, V] = {
							val total = map.getOrElse(key, m.empty) |+| value
							map + (key -> total)
						}

				def merge(map1: Map[K, V], map2: Map[K, V])
					(implicit b: BoundedSemiLattice[V]): Map[K, V] =
						map1 |+| map2
				
				def total(map: Map[K, V])
					(implicit m: Monoid[V]): V =
						map.values.toList.combineAll
			}
    ---------------------------------------------------------------
		
		You should be able to use your instance as follows:
*/
		import sandbox.usecases.crdt.Counter3._
		import cats.instances.int._ // for Monoid

		val g1 = Map("a" -> 7, "b" -> 3)  //> g1  : scala.collection.immutable.Map[String,Int] = Map(a -> 7, b -> 3)
		val g2 = Map("a" -> 2, "b" -> 5)  //> g2  : scala.collection.immutable.Map[String,Int] = Map(a -> 2, b -> 5)
		val counter = GCounter[Map, String, Int]
                                                  //> counter  : sandbox.usecases.crdt.Counter3.GCounter[Map,String,Int] = sandb
                                                  //| ox.usecases.crdt.Counter3$$anon$1@737996a0

		val merged = counter.merge(g1, g2)//> merged  : Map[String,Int] = Map(a -> 7, b -> 5)

		val total = counter.total(merged) //> total  : Int = 12

/*
		The implementation strategy for the type class instance is a bit unsatisfying.
		Although the structure of the implementation will be the same for most instances
		we define, we won’t get any code reuse.

		11.5 Abstracting a Key Value Store

		One solution is to capture the idea of a key-value store within a type class, and
		then generate GCounter instances for any type that has a KeyValueStore
		instance. Here’s the code for such a type class:

			trait KeyValueStore[F[_,_]] {
				def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

				def get[K, V](f: F[K, V])(k: K): Option[V]

				def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
					get(f)(k).getOrElse(default)

				def values[K, V](f: F[K, V]): List[V]
			}

		Implement your own instance for Map.
	
		See the solution
	
	  ----------------------- solution ------------------------------
			K.5 Abstracting a Key Value Store
			Here’s the code for the instance. Write the definition in the companion object
			for KeyValueStore to place it in global implicit scope:

			implicit val mapInstance: KeyValueStore[Map] =
				new KeyValueStore[Map] {
					def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
						f + (k -> v)
	
					def get[K, V](f: Map[K, V])(k: K): Option[V] =
						f.get(k)
		
					override def getOrElse[K, V](f: Map[K, V])
						(k: K, default: V): V =
							f.getOrElse(k, default)
					
					def values[K, V](f: Map[K, V]): List[V] =
					f.values.toList
				}
    ---------------------------------------------------------------
    
		With our type class in place we can implement syntax to enhance data types
		for which we have instances:
	
		implicit class KvsOps[F[_,_], K, V](f: F[K, V]) {
			def put(key: K, value: V)
						(implicit kvs: KeyValueStore[F]): F[K, V] =
							kvs.put(f)(key, value)
			
			def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
						kvs.get(f)(key)
			
			def getOrElse(key: K, default: V)
						(implicit kvs: KeyValueStore[F]): V =
							kvs.getOrElse(f)(key, default)
			
			def values(implicit kvs: KeyValueStore[F]): List[V] =
						kvs.values(f)
		}

		Now we can generate GCounter instances for any data type that has instances
		of KeyValueStore and Monoid using an implicit def:

		implicit def gcounterInstance[F[_,_], K, V]
				(implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]) =
			new GCounter[F, K, V] {
				def increment(f: F[K, V])(key: K, value: V)
							(implicit m: Monoid[V]): F[K, V] = {
					val total = f.getOrElse(key, m.empty) |+| value
					f.put(key, total)
				}
				
				def merge(f1: F[K, V], f2: F[K, V])
							(implicit b: BoundedSemiLattice[V]): F[K, V] =
								f1 |+| f2
				
				def total(f: F[K, V])(implicit m: Monoid[V]): V =
							f.values.combineAll
			}
			
		The complete code for this case study is quite long, but most of it is boilerplate
		setting up syntax for operations on the type class. We can cut down on this
		using compiler plugins such as Simulacrum and Kind Projector.

		11.6 Summary

		In this case study we’ve seen how we can use type classes to model a simple
		CRDT, the GCounter, in Scala. Our implementation gives us a lot of flexibility
		and code reuse: we aren’t tied to the data type we “count”, nor to the data
		type that maps machine IDs to counters.

		The focus in this case study has been on using the tools that Scala provides,
		not on exploring CRDTs. There are many other CRDTs, some of which operate
		in a similar manner to the GCounter, and some of which have very different
		implementations. A fairly recent survey gives a good overview of many of the
		basic CRDTs. However this is an active area of research and we encourage you
		to read the recent publications in the field if CRDTs and eventually consistency
		interest you.
	*/
	
}
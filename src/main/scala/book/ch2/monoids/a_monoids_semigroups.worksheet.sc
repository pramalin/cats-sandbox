// package book.ch2.monoids

// oject a_monoids_semigroups {
  println("Welcome to the Scala worksheet")       

	/*
		Chapter 2

		Monoids and Semigroups

		In this section we explore our first type classes, monoid and semigroup. These
		allow us to add or combine values. There are instances for Ints, Strings,
		Lists, Options, and many more. Let’s start by looking at a few simple types
		and operations to see what common principles we can extract.
		
		Integer addition
		
		Addition of Ints is a binary operation that is closed, meaning that adding two
		Ints always produces another Int:
	*/
	
		2 + 1                             
		
	/*
		There is also the identity element 0 with the property that a + 0 == 0 + a
		== a for any Int a:
	*/
	
		2 + 0                             

		0 + 2                             
	
	/*
		There are also other properties of addition. For instance, it doesn’t matter in
		what order we add elements because we always get the same result. This is a
		property known as associativity:
	*/
		
		(1 + 2) + 3                       

		1 + (2 + 3)                       
		
	/*
		Integer multiplication
		The same properties for addition also apply for multiplication, provided we
		use 1 as the identity instead of 0:
	*/
		
		1 * 3                             

		3 * 1                             

	/*
		Multiplication, like addition, is associative:
	*/
	
		(1 * 2) * 3                       

		1 * (2 * 3)                       
	
	/*
		String and sequence concatenation
		We can also add Strings, using string concatenation as our binary operator:
	*/
	
		"One" ++ "two"                    

	/*
		and the empty string as the identity:
	*/
				
		"" ++ "Hello"                     
		"Hello" ++ ""                     

	/*
		Once again, concatenation is associative:
	*/
		
		("One" ++ "Two") ++ "Three"       

		"One" ++ ("Two" ++ "Three")       

	/*
		Note that we used ++ above instead of the more usual + to suggest a parallel
		with sequences. We can do the same with other types of sequence, using
		concatenation as the binary operator and the empty sequence as our identity.

		2.1 Definition of a Monoid

		We’ve seen a number of “addition” scenarios above each with an associative
		binary addition and an identity element. It will be no surprise to learn that this
		is a monoid. Formally, a monoid for a type A is:

			• an operation combine with type (A, A) => A
			• an element empty of type A

		This definition translates nicely into Scala code. Here is a simplified version of
		the definition from Cats:

				trait Monoid[A] {
					def combine(x: A, y: A): A
					def empty: A
				}
		
		In addition to providing the combine and empty operations, monoids must
		formally obey several laws. For all values x, y, and z, in A, combine must be
		associative and empty must be an identity element:

				def associativeLaw[A](x: A, y: A, z: A)
					(implicit m: Monoid[A]): Boolean = {
					m.combine(x, m.combine(y, z)) ==
					m.combine(m.combine(x, y), z)
				}

			def identityLaw[A](x: A)
				(implicit m: Monoid[A]): Boolean = {
				(m.combine(x, m.empty) == x) &&
				(m.combine(m.empty, x) == x)
			}
		Integer subtraction, for example, is not a monoid because subtraction is not
		associative:
	*/
		
		(1 - 2) - 3                       

		1 - (2 - 3)                       

	/*
		In practice we only need to think about laws when we are writing our own
		Monoid instances. Unlawful instances are dangerous because they can yield
		unpredictable results when used with the rest of Cats’ machinery. Most of
		the time we can rely on the instances provided by Cats and assume the library
		authors know what they’re doing.

		2.2 Definition of a Semigroup

		A semigroup is just the combine part of a monoid. While many semigroups
		are also monoids, there are some data types for which we cannot define an
		empty element. For example, we have just seen that sequence concatena-
		tion and integer addition are monoids. However, if we restrict ourselves to
		non-empty sequences and positive integers, we are no longer able to define
		a sensible empty element. Cats has a NonEmptyList data type that has an
		implementation of Semigroup but no implementation of Monoid.
		
		A more accurate (though still simplified) definition of Cats’ Monoid is:

				trait Semigroup[A] {
					def combine(x: A, y: A): A
				}
	
				trait Monoid[A] extends Semigroup[A] {
					def empty: A
				}
		
		We’ll see this kind of inheritance often when discussing type classes. It provides
		modularity and allows us to re-use behaviour. If we define a Monoid
		for a type A, we get a Semigroup for free. Similarly, if a method requires a
		parameter of type Semigroup[B], we can pass a Monoid[B] instead.
	
		2.3 Exercise: The Truth About Monoids
	
		We’ve seen a few examples of monoids but there are plenty more to be found.
		Consider Boolean. How many monoids can you define for this type? For each
		monoid, define the combine and empty operations and convince yourself that
		the monoid laws hold. Use the following definitions as a starting point:
	
				trait Semigroup[A] {
					def combine(x: A, y: A): A
				}
	
				trait Monoid[A] extends Semigroup[A] {
					def empty: A
				}
		
				object Monoid {
					def apply[A](implicit monoid: Monoid[A]) =
						monoid
				}
	
		See the solution
		----------------------- solution ------------------------------
	*/
	/*
		B.1 The Truth About Monoids
		There are four monoids for Boolean! First, we have and with operator && and
		identity true:
	*/
	
		import cats.Monoid
		
		implicit val booleanAndMonoid: Monoid[Boolean] =
			new Monoid[Boolean] {
				def combine(a: Boolean, b: Boolean) = a && b
				def empty = true
		}                                 

		
		/*
		Second, we have or with operator || and identity false:
		*/
		
		implicit val booleanOrMonoid: Monoid[Boolean] =
			new Monoid[Boolean] {
				def combine(a: Boolean, b: Boolean) = a || b
				def empty = false
			}                         

	
		/*
		Third, we have exclusive or with identity false:
		*/
	
		implicit val booleanEitherMonoid: Monoid[Boolean] =
			new Monoid[Boolean] {
				def combine(a: Boolean, b: Boolean) =
					(a && !b) || (!a && b)
		
				def empty = false
		}                                 

	
		/*
			Finally, we have exclusive nor (the negation of exclusive or) with identity true:
		*/
	
		implicit val booleanXnorMonoid: Monoid[Boolean] =
			new Monoid[Boolean] {
				def combine(a: Boolean, b: Boolean) =
					(!a || b) && (a || !b)
		
				def empty = true
		}                                 


	/*
		Showing that the identity law holds in each case is straightforward. Similarly
		associativity of the combine operation can be shown by enumerating
		the cases.
	*/

	/*
    ---------------------------------------------------------------
		2.4 Exercise: All Set for Monoids
		
		What monoids and semigroups are there for sets?
		See the solution
		----------------------- solution ------------------------------
    ---------------------------------------------------------------
	*/
	/*
		B.2 All Set for Monoids
		
		Set union forms a monoid along with the empty set:
	*/
	
		implicit def setUnionMonoid[A]: Monoid[Set[A]] =
			new Monoid[Set[A]] {
				def combine(a: Set[A], b: Set[A]) = a union b
				def empty = Set.empty[A]
		}                                 

	/*
		We need to define setUnionMonoid as a method rather than a value so we
		can accept the type parameter A. Scala’s implicit resolution is fine with this—it
		is capable of determining the correct type parameter to create a Monoid of
		the desired type:
	*/

	implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
		def combine(a: Int, b: Int) = a + b
		def empty = 0
	}                                         


	import cats.instances.set._ // for Set
	
	val intSetMonoid = Monoid[Set[Int]]       


	intSetMonoid.combine(Set(1, 2), Set(2, 3))

	/*
		Set intersection forms a semigroup, but doesn’t form a monoid because it has
		no identity element:
	*/

	import cats.Semigroup // for Semigroup
	
	implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
		new Semigroup[Set[A]] {
			def combine(a: Set[A], b: Set[A]) =
				a intersect b
		}                                 

	/*
		Set complement and set difference are not associative, so they cannot be considered
		for either monoids or semigroups. However, symmetric difference
		(the union less the intersection) does also form a monoid with the empty set:
	*/

		implicit def symDiffMonoid[A]: Monoid[Set[A]] =
			new Monoid[Set[A]] {
				def combine(a: Set[A], b: Set[A]): Set[A] =
					(a diff b) union (b diff a)
				def empty: Set[A] = Set.empty
		}                                 
	/*
    ---------------------------------------------------------------
		2.5 Monoids in Cats
		
		Now we’ve seen what monoids are, let’s look at their implementation in Cats.
		Once again we’ll look at the three main aspects of the implementation: the
		type class, the instances, and the interface.
		
		2.5.1 The Monoid Type Class
		
		The monoid type class is cats.kernel.Monoid, which is aliased as
		cats.Monoid. Monoid extends cats.kernel.Semigroup, which is aliased
		as cats.Semigroup. When using Cats we normally import type classes from
		the cats // package:
		
				import cats.Monoid
				import cats.Semigroup
				
		Cats Kernel?
		
		Cats Kernel is a subproject of Cats providing a small set of typeclasses
		for libraries that don’t require the full Cats toolbox. While these core
		type classes are technically defined in the cats.kernel // package, they
		are all aliased to the cats // package so we rarely need to be aware of the
		distinction.
		
		The Cats Kernel type classes covered in this book are Eq, Semigroup,
		and Monoid. All the other type classes we cover are part of the main
		Cats project and are defined directly in the cats // package.
		
		2.5.2 Monoid Instances

		Monoid follows the standard Cats pattern for the user interface: the companion
		object has an apply method that returns the type class instance for a
		particular type. For example, if we want the monoid instance for String, and
		we have the correct implicits in scope, we can write the following:
	*/
		import cats.Monoid
		import cats.instances.string._ // for Monoid
		
		Monoid[String].combine("Hi ", "there")
                                                  

		Monoid[String].empty              

	/*
		which is equivalent to:
	*/
	
		Monoid.apply[String].combine("Hi ", "there")
                                                  

		Monoid.apply[String].empty        

	/*
		As we know, Monoid extends Semigroup. If we don’t need empty we can
		equivalently write:
	*/
	
		import cats.Semigroup

		Semigroup[String].combine("Hi ", "there")
                                                  
	
	/*
		The type class instances for Monoid are organised under cats.instances in
		the standard way described in Chapter 1. For example, if we want to pull in
		instances for Int we import from cats.instances.int:
	*/

		import cats.Monoid
		import cats.instances.int._ // for Monoid

		Monoid[Int].combine(32, 10)       

	/*
		Similarly, we can assemble a Monoid[Option[Int]] using instances from
		cats.instances.int and cats.instances.option:
	*/
		
		import cats.Monoid
		import cats.instances.int._ // for Monoid
		import cats.instances.option._ // for Monoid

		val a = Option(22)                

		val b = Option(20)                

		Monoid[Option[Int]].combine(a, b) 

	/*
		Refer back to Chapter 1 for a more comprehensive list of imports.

		2.5.3 Monoid Syntax

		Cats provides syntax for the combine method in the form of the |+| operator.
		Because combine technically comes from Semigroup, we access the syntax
		by importing from cats.syntax.semigroup:
	*/
		
		import cats.instances.string._ // for Monoid
		import cats.syntax.semigroup._ // for |+|
		
		val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
                                                  

		import cats.instances.int._ // for Monoid

		val intResult = 1 |+| 2 |+| Monoid[Int].empty
                                                  

	/*
		2.5.4 Exercise: Adding All The Things
		
		The cutting edge SuperAdder v3.5a-32 is the world’s first choice for adding
		together numbers. The main function in the program has signature def
		add(items: List[Int]): Int. In a tragic accident this code is deleted!
		Rewrite the method and save the day!
		
		See the solution
		
			sandbox.monoid.SupperAdder
		----------------------- solution ------------------------------
	*/

	/*
		B.3 Adding All The Things
		
		We can write the addition as a simple foldLeft using 0 and the + operator:
	*/
	
		def add(items: List[Int]): Int =
			items.foldLeft(0)(_ + _)  

	/*
		We can alternatively write the fold using Monoids, although there’s not a compelling
		use case for this yet:
	*/

		import cats.Monoid
		import cats.instances.int._ // for Monoid
		import cats.syntax.semigroup._ // for |+|
		
		def add2(items: List[Int]): Int =
			items.foldLeft(Monoid[Int].empty)(_ |+| _)
                                                  
	/*
    ---------------------------------------------------------------

		Well done! SuperAdder’s market share continues to grow, and now
		there is demand for additional functionality. People now want to add
		List[Option[Int]]. Change add so this is possible. The SuperAdder code
		base is of the highest quality, so make sure there is no code duplication!
		
		See the solution
		----------------------- solution ------------------------------
	*/
	/*
		B.4 Adding All The Things Part 2

		Now there is a use case for Monoids. We need a single method that adds
		Ints and instances of Option[Int]. We can write this as a generic method
		that accepts an implicit Monoid as a parameter:
	*/

		import cats.Monoid
		import cats.instances.int._ // for Monoid
		import cats.syntax.semigroup._ // for |+|
		
		def add3[A](items: List[A])(implicit monoid: Monoid[A]): A =
			items.foldLeft(monoid.empty)(_ |+| _)
                                                  
		
		/*
			We can optionally use Scala’s context bound syntax to write the same code in
			a friendlier way:
		*/
		
		def add4[A: Monoid](items: List[A]): A =
			items.foldLeft(Monoid[A].empty)(_ |+| _)
                                                  
		/*
			We can use this code to add values of type Int and Option[Int] as requested:
		*/
		
		import cats.instances.int._ // for Monoid
		
		add4(List(1, 2, 3))               

		import cats.instances.option._ // for Monoid
		
		add4(List(Some(1), None, Some(2), None, Some(3)))
                                                  

		/*
		Note that if we try to add a list consisting entirely of Some values, we get a
		compile error:
		*/

	/*
		add2(List(Some(1), Some(2), Some(3)))

		Multiple markers at this line
		- not enough arguments for method add2: (implicit evidence$1: cats.Monoid[Some[Int]])Some[Int].  Unspecified value parameter evidence$1.
		- could not find implicit value for evidence parameter of type cats.Monoid[Some[Int]]

		This happens because the inferred type of the list is List[Some[Int]], while
		Cats will only generate a Monoid for Option[Int]. We’ll see how to get
		around this in a moment.
*/
		
/*
    ---------------------------------------------------------------
		
		SuperAdder is entering the POS (point-of-sale, not the other POS) market.
		Now we want to add up Orders:
		
*/
		case class Order(totalCost: Double, quantity: Double)
/*
		We need to release this code really soon so we can’t make any modifications
		to add. Make it so!
		See the solution
	
		----------------------- solution ------------------------------

		B.5 Adding All The Things Part 3

		Easy—we simply define a monoid instance for Order!
	*/

		implicit val monoid: Monoid[Order] = new Monoid[Order] {
			def combine(o1: Order, o2: Order) =
				Order(
					o1.totalCost + o2.totalCost,
					o1.quantity + o2.quantity
				)

			def empty = Order(0, 0)
		}                                 


	/*
    ---------------------------------------------------------------

		2.6 Applications of Monoids

		We now know what a monoid is—an abstraction of the concept of adding or
		combining—but where is it useful? Here are a few big ideas where monoids
		play a major role. These are explored in more detail in case studies later in the
		book.

		2.6.1 Big Data
		
		In big data applications like Spark and Hadoop we distribute data analysis over
		many machines, giving fault tolerance and scalability. This means each machine
		will return results over a portion of the data, and we must then combine
		these results to get our final result. In the vast majority of cases this can be
		viewed as a monoid.
		
		If we want to calculate how many total visitors a web site has received, that
		means calculating an Int on each portion of the data. We know the monoid
		instance of Int is addition, which is the right way to combine partial results.

		If we want to find out how many unique visitors a website has received, that’s
		equivalent to building a Set[User] on each portion of the data. We know the
		monoid instance for Set is the set union, which is the right way to combine
		partial results.

		If we want to calculate 99% and 95% response times from our server logs, we
		can use a data structure called a QTree for which there is a monoid.
		Hopefully you get the idea. Almost every analysis that we might want to do
		over a large data set is a monoid, and therefore we can build an expressive
		and powerful analytics system around this idea. This is exactly what Twitter’s
		Algebird and Summingbird projects have done. We explore this idea further
		in the map-reduce case study.

		2.6.2 Distributed Systems

		In a distributed system, different machines may end up with different views of
		data. For example, one machine may receive an update that other machines
		did not receive. We would like to reconcile these different views, so every
		machine has the same data if no more updates arrive. This is called eventual
		consistency.

		A particular class of data types support this reconciliation. These data types
		are called commutative replicated data types (CRDTs). The key operation is
		the ability to merge two data instances, with a result that captures all the informa
		tion in both instances. This operation relies on having a monoid instance.
		We explore this idea further in the CRDT case study.

		2.6.3 Monoids in the Small

		The two examples above are cases where monoids inform the entire system
		architecture. There are also many cases where having a monoid around makes
		it easier to write a small code fragment. We’ll see lots of examples in the case
		studies in this book.

		2.7 Summary

		We hit a big milestone in this chapter—we covered our first type classes with
		fancy functional programming names:

			• a Semigroup represents an addition or combination operation;
			• a Monoid extends a Semigroup by adding an identity or “zero” element.

		We can use Semigroups and Monoids by importing three things: the type
		classes themselves, the instances for the types we care about, and the semigroup
		syntax to give us the |+| operator:
*/

		import cats.Monoid
		import cats.instances.string._ // for Monoid
		import cats.syntax.semigroup._ // for |+|

		"Scala" |+| " with " |+| "Cats"   

	/*
		With the correct instances in scope, we can set about adding anything we
		want:
	*/
	
		import cats.instances.int._ // for Monoid
		import cats.instances.option._ // for Monoid

		Option(1) |+| Option(2)           

		import cats.instances.map._ // for Monoid

		val map1 = Map("a" -> 1, "b" -> 2)
		val map2 = Map("b" -> 3, "d" -> 4)
		map1 |+| map2                     

		import cats.instances.tuple._ // for Monoid

		val tuple1 = ("hello", 123)       
		val tuple2 = ("world", 321)       
		tuple1 |+| tuple2                 

	/*
		We can also write generic code that works with any type for which we have
		an instance of Monoid:
	*/
	
		def addAll[A](values: List[A])
				(implicit monoid: Monoid[A]): A =
			values.foldRight(monoid.empty)(_ |+| _)
                                                  

		addAll(List(1, 2, 3))             

		addAll(List(None, Some(1), Some(2)))
                                                  

	/*
		Monoids are a great gateway to Cats. They’re easy to understand and simple
		to use. However, they’re just the tip of the iceberg in terms of the abstractions
		Cats enables us to make. In the next chapter we’ll look at functors, the type
		class personification of the beloved map method. That’s where the fun really
		begins!
	*/
	
// }
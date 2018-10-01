package book.ch6

object h_apply_applicatives {
	/*
	 6.5 Apply and Applictive
	Semigroupals aren’t mentioned frequently in the wider functional programming
	literature. They provide a subset of the functionality of a related type
	class called an applicative functor (“applicative” for short).
	
	Semigroupal and Applicative effectively provide alternative encodings of
	the same notion of joining contexts. Both encodings are introduced in the
	same 2008 paper by Conor McBride and Ross Paterson (Semigroupal is referred to as “monoidal” in the paper.).
	
	Cats models applicatives using two type classes. The first, cats.Apply, extends
	Semigroupal and Functor and adds an ap method that applies a parameter
	to a function within a context. The second, cats.Applicative, extends
	Apply, adds the pure method introduced in Chapter 4. Here’s a simplified
	definition in code:
	
		trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
			def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
			
			def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
				ap(map(fa)(a => (b: B) => (a, b)))(fb)
		}
	
		trait Applicative[F[_]] extends Apply[F] {
			def pure[A](a: A): F[A]
		}

	Breaking this down, the ap method applies a parameter fa to a function ff
	within a context F[_]. The product method from Semigroupal is defined in
	terms of ap and map.

	Don’t worry too much about the implementation of product—it’s difficult to
	read and the details aren’t particuarly important. The main point is that there
	is a tight relationship between product, ap, and map that allows any one of
	them to be defined in terms of the other two.
	
	Applicative also introduces the pure method. This is the same pure we
	saw in Monad. It constructs a new applicative instance from an unwrapped
	value. In this sense, Applicative is related to Apply as Monoid is related to
	Semigroup.
	
	6.5.1 The Hierarchy of Sequencing Type Classes
	With the introduction of Apply and Applicative, we can zoom out and see
	a whole family of type classes that concern themselves with sequencing computa
	tions in different ways. Figure 6.1 shows the relationship between the
	type classes covered in this book.
	(See Rob Norris’ infographic for a the complete picture. https://github.com/tpolecat/cats-infographic)
	
	  +-------------+    +------------+
    |  Cartesian  |    |  Functor   |
    |             |    |            |
    |   product   |    |    map     |
    +------+------+    +------+-----+
           |                  |
           |  +------------+  |
           +--+   Apply    +--+
              |            |
           +--+    ap      +--+
           |  +------------+  |
           |                  |
    +------+------+    +------+-----+
    | Applicative |    |  FlatMap   |
    |             |    |            |
    |    pure     |    |  flatMap   |
    +------+------+    +------+-----+
           |                  |
           |  +-----------+   |
           +--+           +---+
              |   Monad   |
              |           |
              +-----------+

  Figure 6.1 Monad type class hierarchy
	
	Each type class in the hierarchy represents a particular set of sequencing seman
	tics, introduces a set of characteristic methods, and defines the functionality
	of its supertypes in terms of them:
		• every monad is an applicative;
		• every applicative a semigroupal;
		• and so on.
		
	Because of the lawful nature of the relationships between the type classes,
	the inheritance relationships are constant across all instances of a type class.
	Apply defines product in terms of ap and map; Monad defines product, ap,
	and map, in terms of pure and flatMap.
	
	To illustrate this let’s consider two hypothetical data types:
		• Foo is a monad. It has an instance of the Monad type class that implements
		pure and flatMap and inherits standard definitions of product,
		map, and ap;
		• Bar is an applicative functor. It has an instance of Applicative that
		implements pure and ap and inherits standard definitions of product
		and map.

	What can we say about these two data types without knowing more about
	their implementation?

	We know strictly more about Foo than Bar: Monad is a subtype of Applicative,
	so we can guarantee properties of Foo (namely flatMap) that we cannot
	guarantee with Bar. Conversely, we know that Bar may have a wider range
	of behaviours than Foo. It has fewer laws to obey (no flatMap), so it can
	implement behaviours that Foo cannot.

	This demonstrates the classic trade-off of power (in the mathematical sense)
	versus constraint. The more constraints we place on a data type, the more
	guarantees we have about its behaviour, but the fewer behaviours we can
	model.

	Monads happen to be a sweet spot in this trade-off. They are flexible enough
	to model a wide range of behaviours and restrictive enough to give strong guarantees
	about those behaviours. However, there are situations where monads
	aren’t the right tool for the job. Sometimes we want Thai food, and burritos
	just won’t satisfy.
	
	Whereas monads impose a strict sequencing on the computations they model,
	applicatives and semigroupals impose no such restriction. This puts them in a
	different sweet spot in the hierarchy. We can use them to represent classes
	of parallel / independent computations that monads cannot.
	
	We choose our semantics by choosing our data structures. If we choose a
	monad, we get strict sequencing. If we choose an applicative, we lose the
	ability to flatMap. This is the trade-off enforced by the consistency laws. So
	choose your types carefully!
	
	6.6 Summary
	While monads and functors are the most widely used sequencing data types
	we’ve covered in this book, semigroupals and applicatives are the most general.
	These type classes provide a generic mechanism to combine values and apply
	functions within a context, from which we can fashion monads and a variety
	of other combinators.
	
	Semigroupal and Applicative are most commonly used as a means of combining
	independent values such as the results of valida􀦞on rules. Cats provides
	the Validated type for this specific purpose, along with apply syntax as a convenient
	way to express the combination of rules.

	We have almost covered all of the functional programming concepts on our
	agenda for this book. The next chapter covers Traverse and Foldable, two
	powerful type classes for converting between data types. Ateer that we’ll look
	at several case studies that bring together all of the concepts from Part I.
*/
	println("empty worksheet")                //> empty worksheet
}
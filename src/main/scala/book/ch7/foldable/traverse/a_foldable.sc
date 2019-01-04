package book.ch7.foldable.traverse

object a_foldable {
  /*
		Chapter 7
		
		Foldable and Traverse

		In this chapter we’ll look at two type classes that capture iteration over collec-
		tions:

			• Foldable abstracts the familiar foldLeft and foldRight operations;
			• Traverse is a higher-level abstraction that uses Applicatives to iterate
				with less pain than folding.

		We’ll start by looking at Foldable, and then examine cases where folding
		becomes complex and Traverse becomes convenient.

		7.1 Foldable

		The Foldable type class captures the foldLeft and foldRight methods
		we’re used to in sequences like Lists, Vectors, and Streams. Using Foldable,
		we can write generic folds that work with a variety of sequence types.
		We can also invent new sequences and plug them into our code. Foldable
		gives us great use cases for Monoids and the Eval monad.

		7.1.1 Folds and Folding

		Let’s start with a quick recap of the general concept of folding. We supply an
		accumulator value and a binary function to combine it with each item in the
		sequence:
	*/

  def show[A](list: List[A]): String =
    list.foldLeft("nil")((accum, item) => s"$item then $accum")
                                                  //> show: [A](list: List[A])String
  show(Nil)                                       //> res0: String = nil
  show(List(1, 2, 3))                             //> res1: String = 3 then 2 then 1 then nil

  /*
		The foldLeft method works recursively down the sequence. Our binary
		function is called repeatedly for each item, the result of each call becoming
		the accumulator for the next. When we reach the end of the sequence, the
		final accumulator becomes our final result.

		Depending on the operation we’re performing, the order in which we fold may
		be important. Because of this there are two standard variants of fold:
			• foldLeft traverses from “left” to “right” (start to finish);
			• foldRight traverses from “right” to “left” (finish to start).

           ^                       ^
          / \                     / \
    0 + 1    ^                  1    ^
            / \                 |   / \
      1 + 2    ^                | 2    ^
              / \               | |   / \
        3 + 3    X              | | 3    X
          6                     | | |
                                | | 3  + 0
                                | 2 +  3
                                1 + 5
                                  6

		Figure 7.1 illustrates each direction.
		foldLeft and foldRight are equivalent if our binary operation is commuta
		tive. For example, we can sum a List[Int] by folding in either direction,
		using 0 as our accumulator and addition as our operation:
	*/
  List(1, 2, 3).foldLeft(0)(_ + _)                //> res2: Int = 6
  List(1, 2, 3).foldRight(0)(_ + _)               //> res3: Int = 6

  /*
		7.1.2 Exercise: Reflecting on Folds
		Try using foldLeft and foldRight with an empty list as the accumulator and
		:: as the binary operator. What results do you get in each case?

		Solution:
		----------------------- solution ------------------------------
		G.1 Reflecting on Folds
		Folding from left to right reverses the list:
	*/
  List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a)
                                                  //> res4: List[Int] = List(3, 2, 1)

  // Folding right to left copies the list, leaving the order intact:
  List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a)
                                                  //> res5: List[Int] = List(1, 2, 3)

  /*
		Note that we have to carefully specify the type of the accumulator to avoid
		a type error. We use List.empty[Int] to avoid inferring the accumulator
		type as Nil.type or List[Nothing]:

			List(1, 2, 3).foldRight(Nil)(_ :: _)
		// <console>:13: error: type mismatch;
		// found : List[Int]
		// required: scala.collection.immutable.Nil.type
		// List(1, 2, 3).foldRight(Nil)(_ :: _)
		//                                ^
	  ---------------------------------------------------------------

		7.1.3 Exercise: Scaf-fold-ing Other Methods
		foldLeft and foldRight are very general methods. We can use them to implement
		many of the other high-level sequence operations we know. Prove
		this to yourself by implementing substitutes for List's map, flatMap, filter,
		and sum methods in terms of foldRight.

		Solution:
		----------------------- solution ------------------------------

		G.2 Scaf-fold-ing Other Methods
		Here are the solutions:
	*/

  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) :: accum
    }                                             //> map: [A, B](list: List[A])(func: A => B)List[B]
  map(List(1, 2, 3))(_ * 2)                       //> res6: List[Int] = List(2, 4, 6)

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) ::: accum
    }                                             //> flatMap: [A, B](list: List[A])(func: A => List[B])List[B]
  flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))
                                                  //> res7: List[Int] = List(1, 10, 100, 2, 20, 200, 3, 30, 300)

  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) { (item, accum) =>
      if (func(item)) item :: accum else accum
    }                                             //> filter: [A](list: List[A])(func: A => Boolean)List[A]
  filter(List(1, 2, 3))(_ % 2 == 1)               //> res8: List[Int] = List(1, 3)

  /*
		We’ve provided two definitions of sum, one using scala.math.Numeric
		(which recreates the built-in functionality accurately)…
	*/

  import scala.math.Numeric
  def sumWithNumeric[A](list: List[A])(implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)    //> sumWithNumeric: [A](list: List[A])(implicit numeric: scala.math.Numeric[A])
                                                  //| A
  sumWithNumeric(List(1, 2, 3))                   //> res9: Int = 6
  /*
		and one using cats.Monoid (which is more appropriate to the content of this
		book):
	*/

  import cats.Monoid
  def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)  //> sumWithMonoid: [A](list: List[A])(implicit monoid: cats.Monoid[A])A
  import cats.instances.int._ // for Monoid
  sumWithMonoid(List(1, 2, 3))                    //> res10: Int = 6

	/*
	  ---------------------------------------------------------------
	*/
	
}
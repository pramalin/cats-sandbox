package book.ch7

object b_foldable_in_cats {
  /*
		7.1.4 Foldable in Cats
		Cats’ Foldable abstracts foldLeft and foldRight into a type class. Instances
		of Foldable define these two methods and inherit a host of derived
		methods. Cats provides out-of-the-box instances of Foldable for a handful
		of Scala data types: List, Vector, Stream, and Option.

		We can summon instances as usual using Foldable.apply and call their implementa
		tions of foldLeft directly. Here is an example using List:
	*/
  import cats.Foldable
  import cats.instances.list._ // for Foldable
  val ints = List(1, 2, 3)                        //> ints  : List[Int] = List(1, 2, 3)
  Foldable[List].foldLeft(ints, 0)(_ + _)         //> res0: Int = 6

  /*
		Other sequences like Vector and Stream work in the same way. Here is an
		example using Option, which is treated like a sequence of zero or one elements:
	*/

	import cats.instances.option._ // for Foldable
  val maybeInt = Option(123)                      //> maybeInt  : Option[Int] = Some(123)
  Foldable[Option].foldLeft(maybeInt, 10)(_ * _)  //> res1: Int = 1230

  /*
		7.1.4.1 Folding Right
		Foldable defines foldRight differently to foldLeft, in terms of the Eval
		monad:

		  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

		Using Eval means folding is always stack safe, even when the collection’s default
		definition of foldRight is not. For example, the default implementation
		of foldRight for Stream is not stack safe. The longer the stream, the larger
		the stack requirements for the fold. A sufficiently large stream will trigger a
		StackOverflowError:
	*/

  import cats.Eval
  import cats.Foldable
  def bigData = (1 to 100000).toStream            //> bigData: => scala.collection.immutable.Stream[Int]
//  bigData.foldRight(0L)(_ + _)
// java.lang.StackOverflowError

  /*
		Using Foldable forces us to use stack safe opera􀦞ons, which fixes the overflow
		exception:
	*/

  import cats.instances.stream._ // for Foldable
  val eval: Eval[Long] =
    Foldable[Stream].
      foldRight(bigData, Eval.now(0L)) { (num, eval) =>
        eval.map(_ + num)
      }                                           //> eval  : cats.Eval[Long] = cats.Eval$$anon$9@3e6fa38a
  eval.value                                      //> res2: Long = 5000050000

  /*
		Stack Safety in the Standard Library
		Stack safety isn’t typically an issue when using the standard library. The
		most commonly used collec􀦞on types, such as List and Vector, provide
		stack safe implementa􀦞ons of foldRight:
	*/
  (1 to 100000).toList.foldRight(0L)(_ + _)       //> res3: Long = 5000050000

  (1 to 100000).toVector.foldRight(0L)(_ + _)     //> res4: Long = 5000050000

  /*
		We’ve called out Stream because it is an exception to this rule. What
		ever data type we’re using, though, it’s useful to know that Eval has our
		back.
	*/

  /*
		7.1.4.2 Folding with Monoids
		Foldable provides us with a host of useful methods defined on top of
		foldLeft. Many of these are facimiles of familiar methods from the standard
		library: find, exists, forall, toList, isEmpty, nonEmpty, and so on:
	*/
  Foldable[Option].nonEmpty(Option(42))           //> res5: Boolean = true

  Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)  //> res6: Option[Int] = Some(2)

  /*
		In addition to these familiar methods, Cats provides two methods that make
		use of Monoids:
			• combineAll (and its alias fold) combines all elements in the sequence
			using their Monoid;
			• foldMap maps a user-supplied function over the sequence and combines
			the results using a Monoid.

		For example, we can use combineAll to sum over a List[Int]:
	*/
		import cats.instances.int._ // for Monoid
		Foldable[List].combineAll(List(1, 2, 3))
                                                  //> res7: Int = 6

	/*
		Alternatively, we can use foldMap to convert each Int to a String and concatenate
		them:
	*/

  import cats.instances.string._ // for Monoid
  Foldable[List].foldMap(List(1, 2, 3))(_.toString)
                                                  //> res8: String = 123

  /*
		Finally, we can compose Foldables to support deep traversal of nested sequences:
	*/

  import cats.instances.vector._ // for Monoid
  val ints2 = List(Vector(1, 2, 3), Vector(4, 5, 6))
                                                  //> ints2  : List[scala.collection.immutable.Vector[Int]] = List(Vector(1, 2, 3
                                                  //| ), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(ints2)
                                                  //> res9: Int = 21

  /*
		Every method in Foldable is available in syntax form via
		cats.syntax.foldable. In each case, the first argument to the method on
		Foldable becomes the receiver of the method call:
	*/
  import cats.syntax.foldable._ // for combineAll and foldMap
  List(1, 2, 3).combineAll                        //> res10: Int = 6

  List(1, 2, 3).foldMap(_.toString)               //> res11: String = 123

	/*
		Explicits over Implicits
		Remember that Scala will only use an instance of Foldable if the
		method isn’t explicitly available on the receiver. For example, the following
		code will use the version of foldLeft defined on List:

			List(1, 2, 3).foldLeft(0)(_ + _)
		// res18: Int = 6

		whereas the following generic code will use Foldable:

			import scala.language.higherKinds
			def sum[F[_]: Foldable](values: F[Int]): Int =
			values.foldLeft(0)(_ + _)
			// sum: [F[_]](values: F[Int])(implicit evidence$1: cats.
			Foldable[F])Int

		We typically don’t need to worry about this dis􀦞nc􀦞on. It’s a feature!
		We call the method we want and the compiler uses a Foldable when
		needed to ensure our code works as expected. If we need a stack-safe
		implementation of foldRight, using Eval as the accumulator is enough
		to force the compiler to select the method from Cats.
	*/

}
package book.ch6

object c_semigroupal_others {

  /*
		6.3 Semigroupal Applied to Different Types
		Semigroupal doesn’t always provide the behaviour we expect, particularly
		for types that also have instances of Monad. We have seen the behaviour of
		the Semigroupal for Option. Let’s look at some examples for other types.
		Future
		The semantics for Future provide parallel as opposed to sequential execution:
	*/

  import cats.Semigroupal
  import cats.instances.future._ // for Semigroupal
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.language.higherKinds

  val futurePair = Semigroupal[Future].
    product(Future("Hello"), Future(123))         //> futurePair  : scala.concurrent.Future[(String, Int)] = Future(<not completed
                                                  //| >)
  Await.result(futurePair, 1.second)              //> res0: (String, Int) = (Hello,123)

  /*
		The two Futures start executing the moment we create them, so they are
		already calculating results by the time we call product. We can use apply
		syntax to zip fixed numbers of Futures:
	*/

  import cats.syntax.apply._ // for mapN

  case class Cat(
    name:          String,
    yearOfBirth:   Int,
    favoriteFoods: List[String])

  val futureCat = (
    Future("Garfield"),
    Future(1978),
    Future(List("Lasagne"))).mapN(Cat.apply)      //> futureCat  : scala.concurrent.Future[book.ch6.c_semigroupal_others.Cat] = F
                                                  //| uture(<not completed>)
  Await.result(futureCat, 1.second)               //> res1: book.ch6.c_semigroupal_others.Cat = Cat(Garfield,1978,List(Lasagne))
                                                  //| 

  /*
		List
		Combining Lists with Semigroupal produces some potentially unexpected
		results. We might expect code like the following to zip the lists, but we actually
		get the cartesian product of their elements:
	*/

  import cats.Semigroupal
  import cats.instances.list._ // for Semigroupal
  Semigroupal[List].product(List(1, 2), List(3, 4))
                                                  //> res2: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))
  /*
		This is perhaps surprising. Zipping lists tends to be a more common operation.
		We’ll see why we get this behaviour in a moment.

		Either
		We opened this chapter with a discussion of fail-fast versus accumulating
		error-handling. We might expect product applied to Either to accumulate
		errors instead of fail fast. Again, perhaps surprisingly, we find that product
		implements the same fail-fast behaviour as flatMap:
	*/

  import cats.instances.either._ // for Semigroupal
  type ErrorOr[A] = Either[Vector[String], A]
  Semigroupal[ErrorOr].product(
    Left(Vector("Error 1")),
    Left(Vector("Error 2")))                      //> res3: book.ch6.c_semigroupal_others.ErrorOr[(Nothing, Nothing)] = Left(Vect
                                                  //| or(Error 1))

  /*
		In this example product sees the first failure and stops, even though it is possible
		to examine the second parameter and see that it is also a failure.
	*/

  /*
		6.3.1 Semigroupal Applied to Monads

		The reason for the surprising results for List and Either is that they are
		both monads. To ensure consistent semantics, Cats’ Monad (which extends
		Semigroupal) provides a standard definition of product in terms of map and
		flatMap. This gives what we might think of as unexpected and less useful behaviour
		for a number of data types. The consistency of semantics is important
		for higher level abstractions, but we don’t know about those yet.
		Even our results for Future are a trick of the light. flatMap provides sequen
		tial ordering, so product provides the same. The parallel execution we
		observe occurs because our constituent Futures start running before we call
		product. This is equivalent to the classic create-then-flatMap pattern:
	*/

  val a = Future("Future 1")                      //> a  : scala.concurrent.Future[String] = Future(<not completed>)
  val b = Future("Future 2")                      //> b  : scala.concurrent.Future[String] = Future(<not completed>)
  for {
    x <- a
    y <- b
  } yield (x, y)                                  //> res4: scala.concurrent.Future[(String, String)] = Future(<not completed>)

  /*
		So why bother with Semigroupal at all? The answer is that we can create
		useful data types that have instances of Semigroupal (and Applicative)
		but not Monad. This frees us to implement product in different ways. We’ll
		examine this further in a moment when we look at an alternative data type for
		error handling.
	*/

  /*
		6.3.1.1 Exercise: The Product of Monads

		Implement product in terms of flatMap:
			def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = ???
	*/

  import cats.Monad
  import cats.syntax.flatMap._ // for flatMap
  import cats.syntax.functor._ // for map

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    x.flatMap(a => y.map(b => (a, b)))            //> product: [M[_], A, B](x: M[A], y: M[B])(implicit evidence$2: cats.Monad[M])
                                                  //| M[(A, B)]
	// alternative - with for comprehension
  // Unsurprisingly, this code is equivalent to a for comprehension:
  def product2[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)                                //> product2: [M[_], A, B](x: M[A], y: M[B])(implicit evidence$3: cats.Monad[M]
                                                  //| )M[(A, B)]

  /*
		The semantics of flatMap are what give rise to the behaviour for List and
		Either:
	*/

  product(List(1, 2), List(3, 4))                 //> res5: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))

  import cats.instances.list._ // for Semigroupal
  product(List(1, 2), List(3, 4))                 //> res6: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))

  type ErrorOrInt[A] = Either[Vector[String], A]
  
  product[ErrorOrInt, Int, Int](
    Left(Vector("Error 1")),
    Left(Vector("Error 2")))                      //> res7: book.ch6.c_semigroupal_others.ErrorOrInt[(Int, Int)] = Left(Vector(Er
                                                  //| ror 1))

}
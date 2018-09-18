package book.ch4

object b_monad_cats {

  //
  // 4.2.1 The Monad Type Class
  //
  import cats.Monad
  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad

  val opt1 = Monad[Option].pure(3)                //> opt1  : Option[Int] = Some(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
                                                  //> opt2  : Option[Int] = Some(5)
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)//> opt3  : Option[Int] = Some(500)

  val list1 = Monad[List].pure(3)                 //> list1  : List[Int] = List(3)

  val list2 = Monad[List].
    flatMap(List(1, 2, 3))(a => List(a, a * 10))  //> list2  : List[Int] = List(1, 10, 2, 20, 3, 30)

  val list3 = Monad[List].map(list2)(a => a + 123)//> list3  : List[Int] = List(124, 133, 125, 143, 126, 153)

  //
  // 4.2.2 Default Instances
  //
  //  import cats.instances.option._ // for Monad
  Monad[Option].flatMap(Option(1))(a => Option(a * 2))
                                                  //> res0: Option[Int] = Some(2)

  //  import cats.instances.list._ // for Monad
  Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
                                                  //> res1: List[Int] = List(1, 10, 2, 20, 3, 30)

  import cats.instances.vector._ // for Monad
  Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))
                                                  //> res2: Vector[Int] = Vector(1, 10, 2, 20, 3, 30)

  import cats.instances.future._ // for Monad
  import scala.concurrent._
  import scala.concurrent.duration._

  import scala.concurrent.ExecutionContext.Implicits.global
  /* import of Execution context is required to fix the following error for "val fm = Monad[Future]"

		Multiple markers at this line
		- not enough arguments for method apply: (implicit instance: cats.Monad[scala.concurrent.Future])cats.Monad[scala.concurrent.Future] in object Monad.  Unspecified value parameter instance.
		- could not find implicit value for parameter instance: cats.Monad[scala.concurrent.Future]
	*/

  val fm = Monad[Future]                          //> fm  : cats.Monad[scala.concurrent.Future] = cats.instances.FutureInstances$
                                                  //| $anon$1@29ee9faa

  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
                                                  //> future  : scala.concurrent.Future[Int] = Future(<not completed>)
  Await.result(future, 1.second)                  //> res3: Int = 3

  //
  // Monad Syntax
  //

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap
  import scala.language.higherKinds

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))     //> sumSquare: [F[_]](a: F[Int], b: F[Int])(implicit evidence$2: cats.Monad[F])
                                                  //| F[Int]

  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad

  sumSquare(Option(3), Option(4))                 //> res4: Option[Int] = Some(25)
  sumSquare(List(1, 2, 3), List(4, 5))            //> res5: List[Int] = List(17, 26, 20, 29, 25, 34)

  // for comprehension style
  def sumSquare1[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y                         //> sumSquare1: [F[_]](a: F[Int], b: F[Int])(implicit evidence$3: cats.Monad[F]
                                                  //| )F[Int]
  sumSquare1(Option(3), Option(4))                //> res6: Option[Int] = Some(25)
  sumSquare1(List(1, 2, 3), List(4, 5))           //> res7: List[Int] = List(17, 26, 20, 29, 25, 34)

  //
  // The Identity Monad
  //

  // Note: Id can be used wrap Monad around plain values.

  // sumSquare(3, 4)
  /*
	  Multiple markers at this line
		- type mismatch;  found   : Int(3)  required: F[Int]
		- no type parameters for method sumSquare: (a: F[Int], b: F[Int])(implicit evidence$1: cats.Monad[F])F[Int] exist so that it can be applied to arguments (Int, Int)  --- because --- argument expression's type is not compatible with formal parameter type;  found   : Int  required: ?F[Int]
		- type mismatch;  found   : Int(4)  required: F[Int]
	*/

  import cats.Id
  sumSquare(3: Id[Int], 4: Id[Int])               //> res8: cats.Id[Int] = 25

  /*
  	Here is the defini􀦞on of Id to explain:

		package cats
		type Id[A] = A

		Id is actually a type alias that turns an atomic type into a single-parameter
		type constructor. We can cast any value of any type to a corresponding Id:
	*/

  "Dave": Id[String]                              //> res9: cats.Id[String] = Dave
  // res3: cats.Id[String] = Dave
  123: Id[Int]                                    //> res10: cats.Id[Int] = 123
  // res4: cats.Id[Int] = 123
  List(1, 2, 3): Id[List[Int]]                    //> res11: cats.Id[List[Int]] = List(1, 2, 3)
  // res5: cats.Id[List[Int]] = List(1, 2, 3)

  /*
		Cats provides instances of various type classes for Id, including Functor and
		Monad. These let us call map, flatMap, and pure passing in plain values:
	*/

  val a = Monad[Id].pure(3)                       //> a  : cats.Id[Int] = 3
  val b = Monad[Id].flatMap(a)(_ + 1)             //> b  : cats.Id[Int] = 4

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap
  for {
    x <- a
    y <- b
  } yield x + y                                   //> res12: cats.Id[Int] = 7

}
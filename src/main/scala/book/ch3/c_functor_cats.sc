package book.ch3

object c_functor_cats {

  //
  // 3.3 definition of functor
  //

  /*
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
*/

  //
  // 3.5.1 The Functor Type Class
  //
  /*
		The functor type class is cats.Functor. We obtain instances using the standard
		Functor.apply method on the companion object. As usual, default instances
		are arranged by type in the cats.instances package:
	*/

  // import scala.language.higherKinds
  import cats.Functor
  import cats.instances.list._ // for Functor
  import cats.instances.option._ // for Functor

  val list1 = List(1, 2, 3)                       //> list1  : List[Int] = List(1, 2, 3)

  val list2 = Functor[List].map(list1)(_ * 2)     //> list2  : List[Int] = List(2, 4, 6)
  val option1 = Option(123)                       //> option1  : Option[Int] = Some(123)
  val option2 = Functor[Option].map(option1)(_.toString)
                                                  //> option2  : Option[String] = Some(123)

  /*
		Functor also provides the lift method, which converts a function of type A=> B
		 to one that operates over a functor and has type F[A] => F[B]:
	*/
  val func = (x: Int) => x + 1                    //> func  : Int => Int = book.ch3.c_functor_cats$$$Lambda$13/110992469@13a57a3b
                                                  //| 
  val liftedFunc = Functor[Option].lift(func)     //> liftedFunc  : Option[Int] => Option[Int] = cats.Functor$$Lambda$14/863831416
                                                  //| @59e84876
  liftedFunc(Option(1))                           //> res0: Option[Int] = Some(2)

  //
  // 3.5.2 Functor syntax
  //
  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for map
  val func1 = (a: Int) => a + 1                   //> func1  : Int => Int = book.ch3.c_functor_cats$$$Lambda$15/1638172114@39fb3a
                                                  //| b6
  val func2 = (a: Int) => a * 2                   //> func2  : Int => Int = book.ch3.c_functor_cats$$$Lambda$16/1651945012@7946e1
                                                  //| f4
  val func3 = (a: Int) => a + "!"                 //> func3  : Int => String = book.ch3.c_functor_cats$$$Lambda$17/1007251739@5cc
                                                  //| 7c2a6

  val func4 = func1.map(func2).map(func3)         //> func4  : Int => String = scala.Function1$$Lambda$18/1287712235@457e2f02
  func4(123)                                      //> res1: String = 248!

  // ... contd
}
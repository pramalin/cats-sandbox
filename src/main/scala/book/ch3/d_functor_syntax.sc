package book.ch3

object d_functor_syntax {
  // ... contd
  
  //
  // 3.5.2 Functor syntax
  //
  import cats.Functor
  import cats.syntax.functor._

  // another example
  import cats.instances.option._ // for Functor
  import cats.instances.list._ // for Functor

  def doMath[F[_]](src: F[Int])(implicit functor: Functor[F]): F[Int] =
    src.map(n => n + 1 * 2)                       //> doMath: [F[_]](src: F[Int])(implicit functor: cats.Functor[F])F[Int]

  doMath(Option(20))                              //> res0: Option[Int] = Some(22)
  doMath(List(1, 2, 3))                           //> res1: List[Int] = List(3, 4, 5)
  
/* simplified definition

  implicit class FunctorOps[F[_], A](src: F[A]) {
    def map[B](func: A => B)(implicit functor: Functor[F]): F[B] =
      functor.map(src)(func)
  }
*/
/* The map method of FunctorOps requires an implicit Functor as a parameter.
This means this code will only compile if we have a Functor for expr1 (i.e src: F[A]) in
scope. If we don’t, we get a compiler error.

(.. refferance )

	note: F[_] is a type constructor

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
*/

  final case class Box[A](value: A)
  val box = Box[Int](123)                         //> box  : book.ch3.d_functor_syntax.Box[Int] = Box(123)
  // box.map(value => value + 1) // value map is not a member of book.ch3.functor_syntax.Box[Int]




}
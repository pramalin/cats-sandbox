package book.ch5

object a_compose_monads {
  println("Welcome to the Scala worksheet")

  import cats.Monad
  import cats.syntax.applicative._ // for pure
  import cats.syntax.flatMap._ // for flatMap
  import scala.language.higherKinds

  // Hypothetical example. This won't actually compile:
  def compose[M1[_]: Monad, M2[_]: Monad] = {
    type Composed[A] = M1[M2[A]]
    new Monad[Composed] {
      def pure[A](a: A): Composed[A] =
        a.pure[M2].pure[M1]

      def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
        // Problem! How do we write flatMap?
        ???
    }

	/*
		It is impossible to write a general defini􀦞on of flatMap without knowing
		something about M1 or M2. However, if we do know something about one
		or other monad, we can typically complete this code. For example, if we fix
		M2 above to be Option, a definition of flatMap comes to light:

    def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
      fa.flatMap(_.fold(None.pure[M])(f))
*/

  }
}
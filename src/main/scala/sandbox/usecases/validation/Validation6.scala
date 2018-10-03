package sandbox.usecases.validation

object Validation6 {
  /*
  		J.8 Checks Part 3

			Here’s a minimal definition of andThen and its corresponding AndThen class:
	*/

  import cats.Semigroup
  import cats.data.Validated
  
  sealed trait Check[E, A, B] {

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, that)
  }

  final case class AndThen[E, A, B, C](
    check1: Check[E, A, B],
    check2: Check[E, B, C]) extends Check[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(a).withEither(_.flatMap(b => check2(b).toEither))
  }
}
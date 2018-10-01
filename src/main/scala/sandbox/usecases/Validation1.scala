package sandbox.usecases

object Validation1 {

  /*
    J.4 Basic Combinators Part 4

    The implementation of apply for And is using the pattern for applicative functors.
    Either has an Applicative instance, but it doesn’t have the semantics
    we want/ It fails fast instead of accumula􀦞ng errors.

    If we want to accumulate errors Validated is a more appropriate abstraction.
    As a bonus, we get more code reuse because we can lean on the applicative
    instance of Validated in the implementation of apply.

    Here’s the complete implementation:
	*/
  
  import cats.Semigroup
  import cats.syntax.either._ // for asLeft and asRight
  import cats.syntax.semigroup._ // for |+|

  final case class CheckF[E, A](func: A => Either[E, A]) {

    def apply(a: A): Either[E, A] =
      func(a)

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(e1), Left(e2))   => (e1 |+| e2).asLeft
          case (Left(e), Right(a))    => e.asLeft
          case (Right(a), Left(e))    => e.asLeft
          case (Right(a1), Right(a2)) => a.asRight
        }
      }
  }

  /*
    Now let’s see another implementation strategy. In this approach we model
    checks as an algebraic data type, with an explicit data type for each combinator.
    We’ll call this implementation Check:
  */
  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          (left(a), right(a)) match {
            case (Left(e1), Left(e2))   => (e1 |+| e2).asLeft
            case (Left(e), Right(a))    => e.asLeft
            case (Right(a), Left(e))    => e.asLeft
            case (Right(a1), Right(a2)) => a.asRight
          }
      }
  }

  final case class And[E, A](
    left:  Check[E, A],
    right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](
    func: A => Either[E, A]) extends Check[E, A]

}

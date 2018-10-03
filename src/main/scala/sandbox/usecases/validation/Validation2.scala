package sandbox.usecases.validation

object Validation2 {
  /*
J.5 Basic Combinators Part 5
This reuses the same technique for and. We have to do a bit more work in
the apply method. Note that it’s OK to short-circuit in this case because the
choice of rules is implicit in the seman􀦞cs of “or”.
  */

  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._ // for mapN
  import cats.data.Validated._ // for Valid and Invalid
  sealed trait Check[E, A] {
    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)
    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(a) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a)    => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  final case class And[E, A](
    left: Check[E, A],
    right: Check[E, A]) extends Check[E, A]

  final case class Or[E, A](
    left:  Check[E, A],
    right: Check[E, A]) extends Check[E, A]
  
  final case class Pure[E, A](
    func: A => Validated[E, A]) extends Check[E, A]

}

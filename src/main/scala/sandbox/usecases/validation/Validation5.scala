package sandbox.usecases.validation

object Validation5 {
  /*
    J.7 Checks Part 2
  
    It’s the same implementation strategy as before with one wrinkle: Validated
    doesn’t have a flatMap method. To implement flatMap we must momentarily
    switch to Either and then switch back to Validated. The withEither
    method on Validated does exactly this. From here we can just follow the
    types to implement apply.
  */

  import cats.Semigroup
  import cats.data.Validated

  sealed trait Check[E, A, B] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]
    def flatMap[C](f: B => Check[E, A, C]) =
      FlatMap[E, A, B, C](this, f)
    // other methods...
  }

  final case class FlatMap[E, A, B, C](
    check: Check[E, A, B],
    func:  B => Check[E, A, C]) extends Check[E, A, C] {
  
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }
  
  // other data types...
}
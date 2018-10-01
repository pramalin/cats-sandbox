package sandbox.usecases

object Validation4 {
  /*
    J.6 Checks
		
		If you follow the same strategy as Predicate you should be able to create
		code similar to the below:
*/
  import cats.Semigroup
  import cats.data.Validated
  import Predicate._
  
  sealed trait Check[E, A, B] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]
    def map[C](f: B => C): Check[E, A, C] =
      Map[E, A, B, C](this, f)
  }
  
  object Check {
    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      Pure(pred)
  }
  
  final case class Map[E, A, B, C](
    check: Check[E, A, B],
    func:  B => C) extends Check[E, A, C] {
  
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).map(func)
  }
  
  final case class Pure[E, A](
    pred: Predicate[E, A]) extends Check[E, A, A] {
    
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(in)
  }
}
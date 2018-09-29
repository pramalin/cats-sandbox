package sandbox.monads
import cats.Monad

/*
	D.12 Branching out Further with Monads
	The code for flatMap is similar to the code for map. Again, we recurse down
	the structure and use the results from func to build a new Tree.
	The code for tailRecM is fairly complex regardless of whether we make it
	tail-recursive or not.
	If we follow the types, the non-tail-recursive solution falls out:
*/
object TreeMonad1 extends App {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    def pure[A](value: A): Tree[A] =
      Leaf(value)
    
    def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(l, r) =>
          Branch(flatMap(l)(func), flatMap(r)(func))
        case Leaf(value) =>
          func(value)
      }
    
    def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] =
      func(arg) match {
        case Branch(l, r) =>
          Branch(
            flatMap(l) {
              case Left(l)  => tailRecM(l)(func)
              case Right(l) => pure(l)
            },
            flatMap(r) {
              case Left(r)  => tailRecM(r)(func)
              case Right(r) => pure(r)
            })
        case Leaf(Left(value)) =>
          tailRecM(value)(func)
        case Leaf(Right(value)) =>
          Leaf(value)
      }
  }
}
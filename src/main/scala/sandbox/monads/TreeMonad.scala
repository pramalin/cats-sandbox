package sandbox.monads
import cats.Monad
import scala.annotation.tailrec

/*
	The tail-recursive solu􀦞on is much harder to write. We adapted this solu-
	tion from this Stack Overflow post by Nazarii Bardiuk. It involves an explicit
	depth first traversal of the tree, maintaining an open list of nodes to visit and
	a closed list of nodes to use to reconstruct the tree:
*/

object TreeMonad {
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

    def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
        open:   List[Tree[Either[A, B]]],
        closed: List[Tree[B]]): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next =>
            l match {
              case Branch(_, _) =>
                loop(l :: r :: next, closed)
              case Leaf(Left(value)) =>
                loop(func(value) :: r :: next, closed)
              case Leaf(Right(value)) =>
                loop(r :: next, pure(value) :: closed)
            }
          case Leaf(Left(value)) :: next =>
            loop(func(value) :: next, closed)
          case Leaf(Right(value)) :: next =>
            closed match {
              case head :: tail =>
                loop(next, Branch(head, pure(value)) :: tail)
              case Nil =>
                loop(next, pure(value) :: closed)
            }
          case Nil =>
            closed
        }
      loop(List(func(arg)), Nil).head
    }
  }

}
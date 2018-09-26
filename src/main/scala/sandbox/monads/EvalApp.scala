package sandbox.monads
import cats.Eval

object EvalApp extends App {
    def foldRightEval[A, B](as: List[A], acc: B)(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        Eval.now(acc)
    }
	def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
		foldRightEval(as, acc) { (a, b) => b.map(fn(a, _))}.value

  println(s"Sum of 0 to 100000 ${foldRight((1 to 100000).toList, 0L)(_ + _)}")

}
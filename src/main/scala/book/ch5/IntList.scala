package book.ch5

  /*
		C.4.10 Solution to: A Better Abstraction Part 5
		We want to generalise the return type of fold. Our starting point is
		
		def fold(end: Int, f: (Int, Int) => Int): Int

		Replacing the return type and tracing it back we arrive at

		def fold[A](list: IntList, f: (Int, A) => A, end: A): A

		where we’ve used a generic type on the method to capture the changing return type. With this we can implement
		double. When we try to do so we’ll see that type inference fails, so we have to give it a bit of help
 */

object IntList extends App {
  sealed trait IntList {
    def fold[A](end: A, f: (Int, A) => A): A =
      this match {
        case End          => end
        case Pair(hd, tl) => f(hd, tl.fold(end, f))
      }
    def length: Int =
      fold[Int](0, (_, tl) => 1 + tl)
    def product: Int =
      fold[Int](1, (hd, tl) => hd * tl)
    def sum: Int =
      fold[Int](0, (hd, tl) => hd + tl)
    def double: IntList =
      fold[IntList](End, (hd, tl) => Pair(hd * 2, tl))
  }
  final case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList
  
  val pair123 = Pair(1, Pair(2, Pair(3, End)))
  
  println(s"pair123.length: ${pair123.length}")
  println(s"pair123.product: ${pair123.product}")
  println(s"pair123.sum: ${pair123.sum}")
  println(s"pair123.double: ${pair123.double}")

}
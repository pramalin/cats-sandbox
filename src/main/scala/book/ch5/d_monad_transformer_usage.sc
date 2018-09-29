package book.ch5

import cats.instances.list._ // for Monad

import cats.data.EitherT
import scala.concurrent.Future

object d_monad_transformer_usage {
  /*
		5.3.5 Usage Patterns
		Widespread use of monad transformers is sometimes difficult because they
		fuse monads together in predefined ways. Without careful thought, we can
		end up having to unpack and repack monads in different configurations to
		operate on them in different contexts.
		
		We can cope with this in multiple ways. One approach involves creating a single
		“super stack” and sticking to it throughout our code base. This works if
		the code simple and largely uniform in nature. For example, in a web applica-
		􀦞on, we could decide that all request handlers are asynchronous and all can
		fail with the same set of HTTP error codes. We could design a custom ADT
		representing the errors and use a fusion Future and Either everywhere in
		our code:
	*/

  sealed abstract class HttpError
  final case class NotFound(item: String) extends HttpError
  final case class BadRequest(msg: String) extends HttpError
  // etc...
  type FutureEither[A] = EitherT[Future, HttpError, A]

  /*
		The “super stack” approach starts to fail in larger, more heterogeneous code
		bases where different stacks make sense in different contexts. Another design
		pattern that makes more sense in these contexts uses monad transformers
		as local “glue code”. We expose untransformed stacks at module boundaries,
		transform them to operate on them locally, and untransform them before passing
		them on. This allows each module of code to make its own decisions about
		which transformers to use:
	*/
  import cats.data.Writer
  
  type Logged[A] = Writer[List[String], A]
  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }                                             //> parseNumber: (str: String)book.ch5.d_monad_transformer_usage.Logged[Option[
                                                  //| Int]]

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
	  import cats.data.OptionT
  
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }                                               //> addAll: (a: String, b: String, c: String)book.ch5.d_monad_transformer_usage
                                                  //| .Logged[Option[Int]]

  // This approach doesn't force OptionT on other users' code:
  val result1 = addAll("1", "2", "3")             //> result1  : book.ch5.d_monad_transformer_usage.Logged[Option[Int]] = WriterT
                                                  //| ((List(Read 1, Read 2, Read 3),Some(6)))
  val result2 = addAll("1", "a", "3")             //> result2  : book.ch5.d_monad_transformer_usage.Logged[Option[Int]] = WriterT
                                                  //| ((List(Read 1, Failed on a),None))
  /*
		Unfortunately, there aren’t one-size-fits-all approaches to working with
		monad transformers. The best approach for you may depend on a lot of factors:
		the size and experience of your team, the complexity of your code base,
		and so on. You may need to experiment and gather feedback from colleagues
		to determine whether monad transformers are a good fit.
	*/
}
package book.ch5

object b_transfamative {
  /* 5.2 A Transformative Example
		Cats provides transformers for many monads, each named with a T suffix: EitherT
		composes Either with other monads, OptionT composes Option, and
		so on.
		Here’s an example that uses OptionT to compose List and Option. We can
		use OptionT[List, A], aliased to ListOption[A] for convenience, to
		transform a List[Option[A]] into a single monad:
	*/
  import cats.data.OptionT
  type ListOption[A] = OptionT[List, A]
  //  import cats.Monad

  /*
		Note how we build ListOption from the inside out: we pass List, the type
		of the outer monad, as a parameter to OptionT, the transformer for the inner
		monad.
		We can create instances of ListOption using the OptionT constructor, or
		more conveniently using pure:
	*/

  import cats.instances.list._ // for Monad
  import cats.syntax.applicative._ // for pure

  val result1: ListOption[Int] = OptionT(List(Option(10)))
                                                  //> result1  : book.ch5.b_transfamative.ListOption[Int] = OptionT(List(Some(10))
                                                  //| )
  val result2: ListOption[Int] = 32.pure[ListOption]
                                                  //> result2  : book.ch5.b_transfamative.ListOption[Int] = OptionT(List(Some(32)
                                                  //| ))
  /*
		The map and flatMap methods combine the corresponding methods of List
		and Option into single opera􀦞ons:
	*/

  result1.flatMap { (x: Int) =>
    result2.map { (y: Int) =>
      x + y
    }
  }                                               //> res0: cats.data.OptionT[List,Int] = OptionT(List(Some(42)))
  
}
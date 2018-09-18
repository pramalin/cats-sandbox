package book.ch3

object b_functor {
	//
	// Cats functor used to provide map() for any Type taking one parameter
	//
  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for map

  val func1: Int => Double =
    (x: Int) => x.toDouble                        //> func1  : Int => Double = book.ch3.b_functor$$$Lambda$3/20132171@2cdf8d8a

  val func2: Double => Double =
    (y: Double) => y * 2                          //> func2  : Double => Double = book.ch3.b_functor$$$Lambda$9/1060830840@7f63425
                                                  //| a

    (func1 map func2)(1) // composition using map //> res0: Double = 2.0

    (func1 andThen func2)(1) // composition using andThen
                                                  //> res1: Double = 2.0

    val v = func2(func1(1)) // composition written out by hand
                                                  //> v  : Double = 2.0

  val func =
    ((x: Int) => x.toDouble).
      map(x => x + 1).
      map(x => x * 2).
      map(x => x + "!")                           //> func  : Int => String = scala.Function1$$Lambda$10/55909012@9f70c54
  func(123)                                       //> res2: String = 248.0!
}
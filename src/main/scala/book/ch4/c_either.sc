package book.ch4
import cats.syntax.either._ // for map and flatMap


object c_either {

  // 4.4.1 Left and Right Bias
  val either1: Either[String, Int] = Right(10)    //> either1  : Either[String,Int] = Right(10)
  val either2: Either[String, Int] = Right(32)    //> either2  : Either[String,Int] = Right(32)

  for {
    a <- either1
    b <- either2
  } yield a + b                                   //> res0: scala.util.Either[String,Int] = Right(42)

  // 4.4.2 Creating Instances
  val a = 3.asRight[String]                       //> a  : Either[String,Int] = Right(3)
  // a: Either[String,Int] = Right(3)
  val b = 4.asRight[String]                       //> b  : Either[String,Int] = Right(4)
  // b: Either[String,Int] = Right(4)
  for {
    x <- a
    y <- b
  } yield x * x + y * y                           //> res1: scala.util.Either[String,Int] = Right(25)

  /*
	def countPositive(nums: List[Int]) =
		nums.foldLeft(Right(0)) { (accumulator, num) =>
			if(num > 0) {
				accumulator.map(_ + 1)
			} else {
				Left("Negative. Stopping!")
		}
	}

	This code fails to compile for two reasons:
		1. the compiler infers the type of the accumulator as Right instead of Either;
		2. we didn’t specify type parameters for Right.apply so the compiler infers the left parameter as Nothing.
	*/

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }                                             //> countPositive: (nums: List[Int])Either[String,Int]

  countPositive(List(1, 2, 3, 4, 5))              //> res2: Either[String,Int] = Right(5)
  countPositive(List(1, -2, 3))                   //> res3: Either[String,Int] = Left(Negative. Stopping!)

  // cats.syntax.either adds some useful extension methods to the Either
  // companion object. The catchOnly and catchNonFatal methods are great
  // for capturing Exceptions as instances of Either:
  Either.catchOnly[NumberFormatException]("foo".toInt)
                                                  //> res4: Either[NumberFormatException,Int] = Left(java.lang.NumberFormatExcept
                                                  //| ion: For input string: "foo")
  Either.catchNonFatal(sys.error("Badness"))      //> res5: Either[Throwable,Nothing] = Left(java.lang.RuntimeException: Badness)
                                                  //| 
  // There are also methods for crea􀦞ng an Either from other data types:
  Either.fromTry(scala.util.Try("foo".toInt))     //> res6: Either[Throwable,Int] = Left(java.lang.NumberFormatException: For inp
                                                  //| ut string: "foo")
  Either.fromOption[String, Int](None, "Badness") //> res7: Either[String,Int] = Left(Badness)
  
  //
  // 4.4.3 Transforming Eithers
  //

  /*  cats.syntax.either also adds some useful methods to instances of Either.
			We can use orElse and getOrElse to extract values from the right side or
			return a default:
	*/

  "Error".asLeft[Int].getOrElse(0)                //> res8: Int = 0
  "Error".asLeft[Int].orElse(2.asRight[String])   //> res9: Either[String,Int] = Right(2)

  // The ensure method allows us to check whether the right-hand value satisfies a predicate:
   //-1.asRight[String].ensure("Must be non-negative!")(_ > 0)
   // TODO this does not work here
  
  // The recover and recoverWith methods provide similar error handling to their namesakes on Future:
  "error".asLeft[Int].recover {
    case str: String => -1
  }                                               //> res10: Either[String,Int] = Right(-1)

  "error".asLeft[Int].recoverWith {
    case str: String => Right(-1)
  }                                               //> res11: Either[String,Int] = Right(-1)


  // There are leftMap and bimap methods to complement map:
  "foo".asLeft[Int].leftMap(_.reverse)            //> res12: Either[String,Int] = Left(oof)
  6.asRight[String].bimap(_.reverse, _ * 7)       //> res13: Either[String,Int] = Right(42)
  "bar".asLeft[Int].bimap(_.reverse, _ * 7)       //> res14: Either[String,Int] = Left(rab)
  
  // The swap method lets us exchange left for right:
  123.asRight[String]                             //> res15: Either[String,Int] = Right(123)
  123.asRight[String].swap                        //> res16: scala.util.Either[Int,String] = Left(123)
}
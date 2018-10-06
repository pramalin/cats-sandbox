package book.ch1.introduction

object eq {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val l1 = List(1, 2, 3).filter(item => item == 1)//> l1  : List[Int] = List(1)
  // val l = List(1, 2, 3).map(Option(_)).filter(item => item == 1)

  import cats.Eq
  import cats.instances.int._ // for Eq
  val eqInt = Eq[Int]                             //> eqInt  : cats.kernel.Eq[Int] = cats.kernel.instances.IntOrder@27efef64

  eqInt.eqv(123, 123)                             //> res0: Boolean = true
  eqInt.eqv(123, 234)                             //> res1: Boolean = false

  // eqInt.eqv(123, "234")	// type mismatch;  found   : String("234")  required: Int
  import cats.syntax.eq._ // for === and =!=

  123 === 123                                     //> res2: Boolean = true
  123 =!= 234                                     //> res3: Boolean = true

  // 123 === "123"			//type mismatch;  found   : String("123")  required: Int

  import cats.instances.int._ // for Eq
  import cats.instances.option._ // for Eq

  // Some(1) === None		// value === is not a member of Some[Int]
  (Some(1): Option[Int]) === (None: Option[Int])  //> res4: Boolean = false

  Option(1) === Option.empty[Int]                 //> res5: Boolean = false

  import cats.syntax.option._ // for some and none
  1.some === none[Int]                            //> res6: Boolean = false
  1.some =!= none[Int]                            //> res7: Boolean = true

  //
	// Comparing custom types
	//
  import java.util.Date
  import cats.instances.long._ // for Eq

  implicit val dateEq: Eq[Date] =
    Eq.instance[Date] { (date1, date2) =>
      date1.getTime === date2.getTime
    }                                             //> dateEq  : cats.Eq[java.util.Date] = cats.kernel.Eq$$anon$106@31ef45e3

  val x = new Date() // now                       //> x  : java.util.Date = Sat Oct 06 15:22:42 EDT 2018
  val y = new Date() // a bit later than now      //> y  : java.util.Date = Sat Oct 06 15:22:42 EDT 2018
  x === x                                         //> res8: Boolean = true
  x === y                                         //> res9: Boolean = false
}
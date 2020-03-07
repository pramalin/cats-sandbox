// package book.ch1.introduction

// oject eq {
  println("Welcome to the Scala worksheet")       
  val l1 = List(1, 2, 3).filter(item => item == 1)
  // val l = List(1, 2, 3).map(Option(_)).filter(item => item == 1)

  import cats.Eq
  import cats.instances.int._ // for Eq
  val eqInt = Eq[Int]                             

  eqInt.eqv(123, 123)                             
  eqInt.eqv(123, 234)                             

  // eqInt.eqv(123, "234")	// type mismatch;  found   : String("234")  required: Int
  import cats.syntax.eq._ // for === and =!=

  123 === 123                                     
  123 =!= 234                                     

  // 123 === "123"			//type mismatch;  found   : String("123")  required: Int

  import cats.instances.int._ // for Eq
  import cats.instances.option._ // for Eq

  // Some(1) === None		// value === is not a member of Some[Int]
  (Some(1): Option[Int]) === (None: Option[Int])  

  Option(1) === Option.empty[Int]                 

  import cats.syntax.option._ // for some and none
  1.some === none[Int]                            
  1.some =!= none[Int]                            

  //
	// Comparing custom types
	//
  import java.util.Date
  import cats.instances.long._ // for Eq

  implicit val dateEq: Eq[Date] =
    Eq.instance[Date] { (date1, date2) =>
      date1.getTime === date2.getTime
    }                                             

  val x = new Date() // now                       
  val y = new Date() // a bit later than now      
  x === x                                         
  x === y                                         
// }
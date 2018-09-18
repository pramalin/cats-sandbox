package book.ch4

object a_monad {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption            //> parseInt: (str: String)Option[Int]
  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)             //> divide: (a: Int, b: Int)Option[Int]

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).flatMap { aNum =>
      parseInt(bStr).flatMap { bNum =>
        divide(aNum, bNum)
      }
    }                                             //> stringDivideBy: (aStr: String, bStr: String)Option[Int]

  stringDivideBy("6", "3")                        //> res0: Option[Int] = Some(2)
  stringDivideBy("6", "0")                        //> res1: Option[Int] = None
  stringDivideBy("6", "foo")                      //> res2: Option[Int] = None
  stringDivideBy("bar", "2")                      //> res3: Option[Int] = None

	// for comprehension implementation
  def stringDivideBy1(aStr: String, bStr: String): Option[Int] =
    for {
      aNum <- parseInt(aStr)
      bNum <- parseInt(bStr)
      ans <- divide(aNum, bNum)
    } yield ans                                   //> stringDivideBy1: (aStr: String, bStr: String)Option[Int]
    

  stringDivideBy1("6", "3")                       //> res4: Option[Int] = Some(2)
  stringDivideBy1("6", "0")                       //> res5: Option[Int] = None
  stringDivideBy1("6", "foo")                     //> res6: Option[Int] = None
  stringDivideBy1("bar", "2")                     //> res7: Option[Int] = None

}
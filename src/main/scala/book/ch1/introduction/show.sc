package book.ch1.introduction

object show {

  import cats.Show
  import cats.instances.int._ // for Show
  import cats.instances.string._ // for Show

  val showInt: Show[Int] = Show.apply[Int]        //> showInt  : cats.Show[Int] = cats.Show$$anon$2@f2a0b8e
  val showString: Show[String] = Show.apply[String]
                                                  //> showString  : cats.Show[String] = cats.Show$$anon$2@1324409e

  val intAsString: String = showInt.show(123)     //> intAsString  : String = 123
  val stringAsString: String = showString.show("abc")
                                                  //> stringAsString  : String = abc

  import cats.syntax.show._ // for show

  val shownInt = 123.show                         //> shownInt  : String = 123
  val shownString = "abc".show                    //> shownString  : String = abc

  // custom instance
  import java.util.Date
  implicit val dateShow: Show[Date] =
    new Show[Date] {
      def show(date: Date): String =
        s"${date.getTime}ms since the epoch."
    }                                             //> dateShow  : cats.Show[java.util.Date] = book.ch1.introduction.show$$anon$1@3
                                                  //| 567135c

	val showDate = dateShow.show(new Date)    //> showDate  : String = 1538853786971ms since the epoch.

	implicit val dateShow2: Show[Date] =
		Show.show(date => s"${date.getTime}ms since the epoch.")
                                                  //> dateShow2  : cats.Show[java.util.Date] = cats.Show$$anon$1@5a61f5df

	dateShow2.show(new Date)                  //> res0: String = 1538853787030ms since the epoch.
	
}
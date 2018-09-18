package book.ch1

object show {

  import cats.Show
  import cats.instances.int._ // for Show
  import cats.instances.string._ // for Show

  val showInt: Show[Int] = Show.apply[Int]        //> showInt  : cats.Show[Int] = cats.Show$$anon$2@1b4fb997
  val showString: Show[String] = Show.apply[String]
                                                  //> showString  : cats.Show[String] = cats.Show$$anon$2@783e6358

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
    }                                             //> dateShow  : cats.Show[java.util.Date] = book.ch1.show$$anon$1@77b52d12

	val showDate = dateShow.show(new Date)    //> showDate  : String = 1536754608676ms since the epoch.

	implicit val dateShow2: Show[Date] =
		Show.show(date => s"${date.getTime}ms since the epoch.")
                                                  //> dateShow2  : cats.Show[java.util.Date] = cats.Show$$anon$1@35fb3008

	dateShow2.show(new Date)                  //> res0: String = 1536754608713ms since the epoch.
	
}
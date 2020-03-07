// package book.ch1.introduction

// oject show {

  import cats.Show
  import cats.instances.int._ // for Show
  import cats.instances.string._ // for Show

  val showInt: Show[Int] = Show.apply[Int]        
  val showString: Show[String] = Show.apply[String]
                                                  

  val intAsString: String = showInt.show(123)     
  val stringAsString: String = showString.show("abc")
                                                  

  import cats.syntax.show._ // for show

  val shownInt = 123.show                         
  val shownString = "abc".show                    

  // custom instance
  import java.util.Date
  implicit val dateShow: Show[Date] =
    new Show[Date] {
      def show(date: Date): String =
        s"${date.getTime}ms since the epoch."
    }                                             


	val showDate = dateShow.show(new Date)    

	implicit val dateShow2: Show[Date] =
		Show.show(date => s"${date.getTime}ms since the epoch.")
                                                  

	dateShow2.show(new Date)                  
	
// }
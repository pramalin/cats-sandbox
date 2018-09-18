package sandbox.typeclasses

object CatShowApp extends App {
  import cats.syntax.show._
  import ShowImplicits._

   val brucyCat = Cat("Brucy", 3, "grey")
   val karuppuCat = Cat("Karuppu", 2, "black")

  println(s"using cats show: ${brucyCat.show}")
  println(s"using cats show: ${karuppuCat.show}")
  karuppuCat.print
  karuppuCat.print
}

package sandbox.typeclasses

object CatPrintableApp extends App {
  import PrintableInstances._

  val brucyCat = Cat("Brucy", 3, "grey")
  val karuppuCat = Cat("Karuppu", 2, "black")

  Printable.print(brucyCat)
  Printable.print(karuppuCat)

  import PrintableSyntax._
  
  brucyCat.print
  karuppuCat.print
}
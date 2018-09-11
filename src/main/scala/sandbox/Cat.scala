package sandbox

final case class Cat(name: String, age: Int, color: String)

object PrintableInstances {
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
         value

      def print(value: String): Unit = println(format(value))
    }

  implicit val intPrintable: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String =
      value.toString

      def print(value: Int): Unit = println(format(value))
    }

  implicit val catPrintable: Printable[Cat] =
    new Printable[Cat] {
     def format(c: Cat): String =
        s"${c.name} is a ${c.age} year-old ${c.color} cat."

      def print(c: Cat): Unit =
        println(format(c))
     }      
}

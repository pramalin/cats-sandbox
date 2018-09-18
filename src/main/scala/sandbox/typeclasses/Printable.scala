package sandbox.typeclasses

trait Printable[A] {
  def format(v: A): String
}
object Printable {
  def format[A](value: A)(implicit instance: Printable[A]): String = {
    instance.format(value)
  }
  def print[A](value: A)(implicit instance: Printable[A]): Unit = {
    println(instance.format(value))
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](v: A) {
    def format(implicit instance: Printable[A]): String = {
      instance.format(v)
    }
    def print(implicit instance: Printable[A]): Unit = {
      println(instance.format(v))
    }
  }
}

object PrintableInstances {
  implicit val printableInt = new Printable[Int] {
    override def format(v: Int): String = s"Int value ${v.toString}"
  }
  implicit val printableString = new Printable[String] {
    override def format(v: String): String = s"String value ${v.toString}"
  }
  implicit val printableCat = new Printable[Cat] {
    override def format(v: Cat): String = s"${v.name} is a ${v.age} year-old ${v.color} cat"
  }
}
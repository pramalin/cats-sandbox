package sandbox

trait Printable[A] {
  def format(value: A): String
  def print(value: A): Unit
}

object Printable {
	def format[A](value: A)(implicit w: Printable[A]): String =
		w.format(value)

  def print[A](value: A)(implicit w: Printable[A]): Unit =
    println(w.format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def print(implicit p: Printable[A]): Unit =
      p.print(value)
  }
}
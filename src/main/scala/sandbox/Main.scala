package sandbox

import cats.instances.string._
import cats.syntax.semigroup._
import JsonWriterInstances._
import JsonSyntax._

import PrintableInstances._
import PrintableSyntax._

object Main extends App {
  println("Hello " |+| "Cats!")


  println(Json.toJson(Person("Dave", "dave@example.com")))

  println(Person("Dave2", "dave@example.com").toJson)

  val cat = Cat("Purr", 2, "gray")

  cat.print
}

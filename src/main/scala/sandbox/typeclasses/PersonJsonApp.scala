package sandbox.typeclasses

object PrintableJsonApp extends App {
  import JsonWriterInstances._

  println(Json.toJson(Person("Dave", "dave@example.com")))

  import JsonSyntax._

  println(Person("Dave2", "dave@example.com").toJson)

}

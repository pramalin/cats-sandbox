// package quick.reference

// oject sheet1 {
  println("Welcome to the Scala worksheet")


  /*
		Implicit classes
		-----------------
		 allows to define extra functionality on existing data types without using conventional inheritance.
	*/

  // example add method yeah to Int
  object IntImplicits {
    implicit class IntOps(n: Int) {
      def yeah() = for { _ <- 0 until n } println("Oh yeah!")
    }
  }

  import IntImplicits._

  3.yeah


	/*
		Type class pattern
		 involve four components:
			• the actual type class itself;
			• the type class instances;
			• interfaces using implicit parameters; and
			• interfaces using enrichment and implicit parameters.
	*/

	/*
		A type class is a trait with at least one type variable. The type variables specify the concrete types the
		type class instances are defined for. Methods in the trait usually use the type variables.
	*/
	trait ExampleTypeClass[A] {
		def doSomething(in: A) = ???
	}

  // Example Define a very simple JSON AST
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  case object JsNull extends Json

  // The "serialize to JSON" behaviour is encoded in this trait
  trait JsonWriter[A] {
    def write(value: A): Json
  }

// }
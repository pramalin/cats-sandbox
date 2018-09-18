package sandbox.typeclasses


  import cats.Eq
  import cats.syntax.eq._ // for === and =!=

  import cats.instances.int._ // for Eq
  import cats.instances.string._ // for Eq



trait EqInstances {
  implicit val catEqInstance: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name &&
        cat1.age === cat2.age &&
        cat1.color === cat2.color
    }
}

object EqInstances extends EqInstances
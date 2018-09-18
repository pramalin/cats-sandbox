package book.ch2

import cats.Monoid
import cats.Semigroup

object monoid {
  println("Welcome to the Scala worksheet")



  //
  // Boolean monoids
  //
  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = x && y

      def empty = true
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = x || y

      def empty = false
    }

  implicit val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = (a && !b) || (!a && b)

      def empty = false
    }

  implicit val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) =
        (!a || b) && (a || !b)
      def empty = true
    }

  //
  // Set monoids
  //
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a union b
      def empty = Set.empty[A]
    }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int) = a + b
    def empty = 0
  }

  val intSetMonoid = Monoid[Set[Int]]
  intSetMonoid.combine(Set(1, 2), Set(2, 3))

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(a: Set[A], b: Set[A]) =
        a intersect b
    }

  /*
  --- These errors reported on line 54 if the symDiffMonoid is defined ---
  - not enough arguments for method apply: (implicit ev: cats.kernel.Monoid[Set[Int]])cats.kernel.Monoid[Set[Int]] in object Monoid.  Unspecified value parameter ev.
  - could not find implicit value for parameter ev: cats.kernel.Monoid[Set[Int]]
  - ambiguous implicit values:  both method symDiffMonoid in object monoid of type [A]=> cats.kernel.Monoid[Set[A]]  and method setUnionMonoid in object monoid of type [A]=> cats.kernel.Monoid[Set[A]]  match expected type cats.kernel.Monoid[Set[Int]]
  */
  /*
  implicit def symDiffMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] =
        (a diff b) union (b diff a)
      def empty: Set[A] = Set.empty
    }
  */
}
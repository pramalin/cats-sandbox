package book.ch2.monoids

import cats.Monoid
import cats.Semigroup

object monoid {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet



  //
  // Boolean monoids
  //
  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = x && y

      def empty = true
    }                                             //> booleanAndMonoid  : cats.Monoid[Boolean] = book.ch2.monoids.monoid$$anon$2@3
                                                  //| ac3fd8b

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(x: Boolean, y: Boolean) = x || y

      def empty = false
    }                                             //> booleanOrMonoid  : cats.Monoid[Boolean] = book.ch2.monoids.monoid$$anon$3@55
                                                  //| 94a1b5

  implicit val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = (a && !b) || (!a && b)

      def empty = false
    }                                             //> booleanEitherMonoid  : cats.Monoid[Boolean] = book.ch2.monoids.monoid$$anon$
                                                  //| 4@6a5fc7f7

  implicit val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) =
        (!a || b) && (a || !b)
      def empty = true
    }                                             //> booleanXnorMonoid  : cats.Monoid[Boolean] = book.ch2.monoids.monoid$$anon$5@
                                                  //| 3b6eb2ec

  //
  // Set monoids
  //
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a union b
      def empty = Set.empty[A]
    }                                             //> setUnionMonoid: [A]=> cats.Monoid[Set[A]]

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int) = a + b
    def empty = 0
  }                                               //> intMonoid  : cats.Monoid[Int] = book.ch2.monoids.monoid$$anon$1@b684286

  val intSetMonoid = Monoid[Set[Int]]             //> intSetMonoid  : cats.kernel.Monoid[Set[Int]] = book.ch2.monoids.monoid$$ano
                                                  //| n$6@573f2bb1
  intSetMonoid.combine(Set(1, 2), Set(2, 3))      //> res0: Set[Int] = Set(1, 2, 3)

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(a: Set[A], b: Set[A]) =
        a intersect b
    }                                             //> setIntersectionSemigroup: [A]=> cats.Semigroup[Set[A]]

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
package sandbox.usecases.crdt

import cats.Monoid
import cats.instances.list._ // for Monoid
import cats.instances.map._ // for Monoid
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._ // for combineAll

/*
	K.3 Generic GCounter

	Here’s a working implementation. Note the use of |+| in the definition of
	merge, which significantly simplifies the process of merging and maximising
	counters:
*/			
object Counter2 {
  
  final case class GCounter[A](counters: Map[String, A]) {
    
    def increment(machine: String, amount: A)(implicit m: Monoid[A]) = {
      val value = amount |+| counters.getOrElse(machine, m.empty)
      GCounter(counters + (machine -> value))
    }

    def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
      GCounter(this.counters |+| that.counters)

    def total(implicit m: Monoid[A]): A =
      this.counters.values.toList.combineAll
  }
}
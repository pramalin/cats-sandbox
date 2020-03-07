// package book.chp11

import cats.Monoid
import sandbox.usecases.crdt.BoundedSemiLattice

// oject b_gcounter_type_class_test {

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])(implicit m: Monoid[V]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
      counter
  }


  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }

  object KeyValueStore {
    implicit val mapInstance: KeyValueStore[Map] =
      new KeyValueStore[Map] {
        def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
          f + (k -> v)

        def get[K, V](f: Map[K, V])(k: K): Option[V] =
          f.get(k)

        override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
          f.getOrElse(k, default)

        def values[K, V](f: Map[K, V]): List[V] =
          f.values.toList
      }
  }

    implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
	    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
	      kvs.put(f)(key, value)
	
	    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
	      kvs.get(f)(key)
	
	    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
	      kvs.getOrElse(f)(key, default)
	
	    def values(implicit kvs: KeyValueStore[F]): List[V] =
	      kvs.values(f)
  }


  /*
			K.5 Abstracting a Key Value Store
			Here’s the code for the instance. Write the definition in the companion object
			for KeyValueStore to place it in global implicit scope:
	*/

  import cats.instances.list._ // for Monoid
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.foldable._ // for combineAll

  implicit val mapInstance: KeyValueStore[Map] =
    new KeyValueStore[Map] {
      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
        f + (k -> v)
      def get[K, V](f: Map[K, V])(k: K): Option[V] =
        f.get(k)
      override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
        f.getOrElse(k, default)
      def values[K, V](f: Map[K, V]): List[V] =
        f.values.toList
    }

  implicit def gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]) =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)(implicit m: Monoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }

      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2

      def total(f: F[K, V])(implicit m: Monoid[V]): V =
        f.values.combineAll
    }
    
 // import sandbox.usecases.crdt.Counter4._
 // import cats.instances.int._ // for Monoid

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

	val counter = GCounter[Map, String, Int]
	
  counter.increment(g2)("b", 1)

  //	GCounter

// }
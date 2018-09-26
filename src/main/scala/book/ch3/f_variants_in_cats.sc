package book.ch3

object f_variants_in_cats {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  /* definition
		trait Contravariant[F[_]] {
			def contramap[A, B](fa: F[A])(f: B => A): F[B]
		}
		trait Invariant[F[_]] {
			def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
		}
	*/

  // 3.7.1 Contravariants in Cats
  import cats.Contravariant
  import cats.Show
  import cats.instances.string._

  val showString = Show[String]                   //> showString  : cats.Show[String] = cats.Show$$anon$2@7f690630
  val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")
                                                  //> showSymbol  : cats.Show[Symbol] = cats.Show$$anon$1@3d8c7aca
  showSymbol.show('dave)                          //> res0: String = 'dave

  import cats.syntax.contravariant._ // for contramap

  showString.contramap[Symbol](_.name).show('dave)//> res1: String = dave

  // 3.7.2 Invariant in Cats
  import cats.Monoid
  import cats.instances.string._ // for Monoid
  import cats.syntax.invariant._ // for imap
  import cats.syntax.semigroup._ // for |+|

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)     //> symbolMonoid  : cats.Monoid[Symbol] = cats.Invariant$$anon$11$$anon$9@685cb1
                                                  //| 37

  Monoid[Symbol].empty                            //> res2: Symbol = '
  
  'a |+| 'few |+| 'words                          //> res3: Symbol = 'afewwords

}
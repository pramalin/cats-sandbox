package sandbox.usecases

object MapReduce {
  import cats.Monoid
  import cats.instances.int._ // for Monoid
  import cats.instances.string._ // for Monoid
  import cats.syntax.semigroup._ // for |+|

  /* initial
  def foldMap[A, B: Monoid](as: Vector[A])(func: A => B): B =
    as.map(func).foldLeft(Monoid[B].empty)(_ |+| _)
	*/

  /*
  	We can make a slight alteration to this code to do everything in one step:
  */
  def foldMap[A, B: Monoid](as: Vector[A])(func: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| func(_))

  /*
    I.3 Implementing parallelFoldMap
    Here is an annotated solution that splits out each map and fold into a separate
    line of code:
  */

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
   
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    // Calculate the number of items to pass to each CPU:
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    // Create one group for each CPU:
    val groups: Iterator[Vector[A]] =
      values.grouped(groupSize)
    // Create a future to foldMap each group:
    val futures: Iterator[Future[B]] =
      groups map { group =>
        Future {
          group.foldLeft(Monoid[B].empty)(_ |+| func(_))
        }
      }
    // foldMap over the groups to calculate a final result:
    Future.sequence(futures) map { iterable =>
      iterable.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  /*
    We can re-use our definition of foldMap for a more concise solution. Note
    that the local maps and reduces in steps 3 and 4 of Figure 9.4 are actually
    equivalent to a single call to foldMap, shortening the entire algorithm as follows:
  */

  def parallelFoldMap1[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    val groups: Iterator[Vector[A]] =
      values.grouped(groupSize)
    val futures: Iterator[Future[B]] =
      groups.map(group => Future(foldMap(group)(func)))
    Future.sequence(futures) map { iterable =>
      iterable.foldLeft(Monoid[B].empty)(_ |+| _)
    }
  }

  /*
  	I.4 parallelFoldMap with more Cats
		We’ll restate all of the necessary imports for completeness:
	*/
  import cats.Monoid
  import cats.Foldable
  import cats.Traverse
  import cats.instances.int._ // for Monoid
  import cats.instances.future._ // for Applicative and Monad
  import cats.instances.vector._ // for Foldable and Traverse
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.foldable._ // for combineAll and foldMap
  import cats.syntax.traverse._ // for traverse
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  /*
    Here’s the implementation of parallelFoldMap delegating as much of the
    method body to Cats as possible:
  */

  def parallelFoldMap2[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.toVector.foldMap(func)))
      .map(_.combineAll)
  }
}
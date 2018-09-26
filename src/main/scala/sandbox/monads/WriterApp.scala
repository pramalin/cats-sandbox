package sandbox.monads

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object WriterApp extends App {
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  val res = Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3)))), 5.seconds)

  println(s"Non-writer result: $res")

  // Exercice 4.7.3 Exercise: Show Your Working
  import cats.data.Writer
  import cats.syntax.writer._ // for writer
  import cats.syntax.applicative._ // for pure
  import cats.instances.vector._ // for Monoid
  
  type Logged[A] = Writer[Vector[String], A]

  def factorialW(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorialW(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  val result = factorialW(5).run
  println(s"vectorLog -> ${result._1.mkString(", ")} result -> ${result._2}")

  val Vector(result1, result2) =
    Await.result(Future.sequence(Vector(
      Future(factorialW(3).run),
      Future(factorialW(5).run))), 5.seconds)

  println(s"factorialW(3) -> ${result1._1.mkString(", ")} result -> ${result1._2}")
  println(s"factorialW(5) -> ${result2._1.mkString(", ")} result -> ${result2._2}")

}
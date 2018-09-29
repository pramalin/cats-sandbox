package sandbox.monads

import scala.concurrent.Future
import cats.data.EitherT

import cats.instances.future._ // for Monad
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Await
import scala.concurrent.duration._

object AutobotApp extends App {
  //  type Response[A] = Future[Either[String, A]]
  // defined type alias Response
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10)

    /*
     The Autobots, well-known robots in disguise, frequently send messages during
      battle requesting the power levels of their team mates. This helps them
      coordinate strategies and launch devastating attacks. The message sending
      method looks like this:
     */
  def getPowerLevel(ally: String): Response[Int] = {
    powerLevels.get(ally) match {
      case Some(avg) => EitherT.right(Future(avg))
      case None      => EitherT.left(Future(s"$ally unreachable"))
    }
  }

  // Two autobots can perform a special move if their combined power level is
  // greater than 15.
  // accepts the names of two allies and checks whether a special move is possible
  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield (power1 + power2) > 15

  // takes two ally names and prints a message saying whether they can perform a special move
  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match {
      case Left(msg) =>
        s"Comms error: $msg"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }

  println(s"""${tacticalReport("Jazz", "Bumblebee")}""")
  println(s"""${tacticalReport("Bumblebee", "Hot Rod")}""")
  println(s"""${tacticalReport("Jazz", "Ironhide")}""")

}
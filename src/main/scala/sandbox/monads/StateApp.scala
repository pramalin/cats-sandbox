package sandbox.monads
import cats.data.State
import cats.syntax.applicative._ // for pure

object StateApp extends App {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case a :: b :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  println(evalOne("42").runA(Nil).value)

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    _ <- evalOne("+")
    _ <- evalOne("3")
    _ <- evalOne("4")
    _ <- evalOne("+")
    ans <- evalOne("*")
  } yield ans

  println(s"program result = ${program.runA(Nil).value}")

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  def evalInput(input: String): Int = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }

  val program2 = evalAll(List("1", "2", "+", "3", "*"))

  println(s"program2 result = ${program2.runA(Nil).value}")

  val program3 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(s"program3 result = ${program3.run(Nil).value}")
  println("evalInput result = " + evalInput("1 2 + 3 4 + *"))

}
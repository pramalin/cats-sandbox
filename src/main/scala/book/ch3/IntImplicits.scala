package book.ch3

object IntImplicits {
  implicit class IntOps(n: Int) {
    def yeah() = for { _ <- 0 until n } println("Oh yeah!")
  }
}

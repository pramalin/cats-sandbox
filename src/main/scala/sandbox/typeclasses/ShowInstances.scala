package sandbox.typeclasses

import cats.Show

trait ShowInstances {
  implicit val intShow: Show[Int] = Show.show[Int](i => s"Int value ${i.toString}")
  implicit val StringShow: Show[String] = Show.show[String](i => s"String value ${i.toString}")
  implicit val catShow: Show[Cat] = Show.show[Cat](cat =>
    s"${cat.name} is a ${cat.age} year-old ${cat.color} cat")
}

trait ShowOps {
  implicit class ShowPrint[A](v: A){
    def print(implicit instance: Show[A]): Unit = {
      println(instance.show(v))
    }
  }
}

object ShowImplicits extends ShowInstances with ShowOps
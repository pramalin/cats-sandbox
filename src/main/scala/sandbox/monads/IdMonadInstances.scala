package sandbox.monads

import cats.Id

object IdMonadInstances {

    def pure[A](a: A): Id[A] = a

    def flatMap[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
    def map[A, B](value: Id[A])(func: A => B): Id[B] = flatMap(value)(func)

}
package book.ch3

import sandbox.functor.PrintableInstances._
import sandbox.functor.Printable
import sandbox.functor.Box

import sandbox.functor.Codec

object e_variants_functor {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  Printable.format("hello")                       //> res0: String = "hello"
  Printable.format(true)                          //> res1: String = yes

  // 3.6.1.1 contramap
  Printable.format(Box[String]("hello world"))    //> res2: String = "hello world"
  Printable.format(Box(true))                     //> res3: String = yes
  //  Printable.format(Box(2.0))

  // 3.6.2.1 imap
  Codec.encode(123.4)                             //> res4: String = 123.4
  Codec.decode[Double]("123.4")                   //> res5: Double = 123.4
  Codec.encode(Box(123.4))                        //> res6: String = 123.4
  Codec.decode[Box[Double]]("123.4")              //> res7: sandbox.functor.Box[Double] = Box(123.4)
 
}
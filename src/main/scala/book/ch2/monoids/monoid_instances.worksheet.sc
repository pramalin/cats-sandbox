// package book.ch2.monoids

// oject monoid_instances {
  import cats.Monoid
  import cats.instances.string._ // for Monoid

  Monoid[String].combine("Hi ", "there")          
  Monoid[String].empty                            

  import cats.Semigroup
  Semigroup[String].combine("Hi ", "there")       

  //  import cats.Monoid
  import cats.instances.int._ // for Monoid
  Monoid[Int].combine(32, 10)                     

  //  import cats.Monoid
//  import cats.instances.int._ // for Monoid
  import cats.instances.option._ // for Monoid
  val a = Option(22)                              
  val b = Option(20)                              
  Monoid[Option[Int]].combine(a, b)               

//  import cats.instances.string._ // for Monoid
  import cats.syntax.semigroup._ // for |+|

  val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
                                                  

//  import cats.instances.int._ // for Monoid

  val intResult = 1 |+| 2 |+| Monoid[Int].empty   

// }
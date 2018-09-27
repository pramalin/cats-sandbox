package book.ch4

object l_readers {
  /*
		4.8.1 Creating and Unpacking Readers

		We can create a Reader[A, B] from a function A => B using the
		Reader.apply constructor:
	*/

  import cats.data.Reader

  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)                       //> catName  : cats.data.Reader[book.ch4.l_readers.Cat,String] = Kleisli(book.ch
                                                  //| 4.l_readers$$$Lambda$3/670700378@5e5792a0)
  /*
		We can extract the function again using the Reader's run method and call it
		using apply as usual:
	*/
  catName.run(Cat("Garfield", "lasagne"))         //> res0: cats.Id[String] = Garfield

  // 4.8.2 Composing Readers

  // The map method simply extends the computa􀦞on in the Reader by passing its
  // result through a function:

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")         //> greetKitty  : cats.data.Reader[book.ch4.l_readers.Cat,String] = Kleisli(cats
                                                  //| .data.Kleisli$$Lambda$11/1620303253@5622fdf)
  greetKitty.run(Cat("Heathcliff", "junk food"))  //> res1: cats.Id[String] = Hello Heathcliff

  /*
  	The flatMap method is more interesting. It allows us to combine readers that
		depend on the same input type. To illustrate this, let’s extend our greeting
		example to also feed the cat:
	*/
  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
                                                  //> feedKitty  : cats.data.Reader[book.ch4.l_readers.Cat,String] = Kleisli(book
                                                  //| .ch4.l_readers$$$Lambda$13/125622176@204f30ec)
  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."                     //> greetAndFeed  : cats.data.Reader[book.ch4.l_readers.Cat,String] = Kleisli(c
                                                  //| ats.data.Kleisli$$Lambda$15/342597804@4dfa3a9d)
  greetAndFeed(Cat("Garfield", "lasagne"))        //> res2: cats.Id[String] = Hello Garfield. Have a nice bowl of lasagne.
  greetAndFeed(Cat("Heathcliff", "junk food"))    //> res3: cats.Id[String] = Hello Heathcliff. Have a nice bowl of junk food.

}
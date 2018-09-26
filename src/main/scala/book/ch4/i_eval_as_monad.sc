package book.ch4

object i_eval_as_monad {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  // 4.6.3 Eval as a Monad
  /*
	  Like all monads, Eval's map and flatMap methods add computa􀦞ons to a
		chain. In this case, however, the chain is stored explicitly as a list of func􀦞ons.
		The func􀦞ons aren’t run un􀦞l we call Eval's value method to request a result:
	*/
  import cats.Eval

  val greeting = Eval.
    always { println("Step 1"); "Hello" }.
    map { str => println("Step 2"); s"$str world" }
                                                  //> greeting  : cats.Eval[String] = cats.Eval$$anon$9@34b7bfc0

  greeting.value                                  //> Step 1
                                                  //| Step 2
                                                  //| res0: String = Hello world

  /*
	Note that, while the semantics of the originating Eval instances are main106
	tained, mapping func􀦞ons are always called lazily on demand (def semantics):
	*/
  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 }
    b <- Eval.always { println("Calculating B"); 2 }
  } yield {
    println("Adding A and B")
    a + b
  }                                               //> Calculating A
                                                  //| ans  : cats.Eval[Int] = cats.Eval$$anon$9@153f5a29
  ans.value                                       //> Calculating B
                                                  //| Adding A and B
                                                  //| res1: Int = 42
  ans.value                                       //> Calculating B
                                                  //| Adding A and B
                                                  //| res2: Int = 42
  /*
	Eval has a memoize method that allows us to memoize a chain of computa-
	tions. The result of the chain up to the call to memoize is cached, whereas
	calculations after the call retain their original semantics:
	(this part doesn't work in worksheet)
	*/
/*
  val saying = Eval.
    always { println("Step 1"); "The cat" }.
    map { str => println("Step 2"); s"$str sat on" }.
    memoize.
    map { str => println("Step 3"); s"$str the mat" }

  saying.value // first access
  saying.value // second access
*/
}
package book.ch4


object h_eval_evoluation {
	// Note: The worksheets doesn't work properly with the Evals. Need to run the snippets in REPL.
  
  // 4.6.2 Eval’s Models of Evalua􀦞on
  
  import cats.Eval
  val now = Eval.now(math.random + 1000)
  val later = Eval.later(math.random + 2000)

  val always = Eval.always(math.random + 3000)
  /*
	We can extract the result of an Eval using its value method:
	now.value
	*/
  later.value
  always.value

  /*
	Each type of Eval calculates its result using one of the evaluation models
	defined above. Eval.now captures a value right now. Its semantics are similar
	to a val—eager and memoized:
	*/
  val x = Eval.now {
    println("Computing X")
    math.random
  }
  // Computing X
  x.value // first access
  x.value // second access

  /*
		Eval.always captures a lazy computation, similar to a def:
	*/
  val y = Eval.always {
    println("Computing Y")
    math.random
  }

  y.value // first access
  y.value // second access
  /*
	Finally, Eval.later captures a lazy, memoized computation, similar to a lazy
	val:
	*/
  val z = Eval.later {
    println("Computing Z")
    math.random
  }
  z.value // first access
  z.value // second access
}
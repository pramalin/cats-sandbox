package book.ch4.monads

object e_eval_monad {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  /*
		4.6 The Eval Monad
		cats.Eval is a monad that allows us to abstract over different models of evalua
		tion. We typically hear of two such models: eager and lazy. Eval throws in
		a further distinction of whether or not a result is memoized.
		
		4.6.1 Eager, Lazy, Memoized, Oh My!
		
		What do these terms mean?

		Eager computations happen immediately whereas lazy computations happen
		on access. Memoized computations are run once on first access, after which
		the results are cached.

		For example, Scala vals are eager and memoized. We can see this using a
		computation with a visible side-effect. In the following example, the code
		to compute the value of x happens at the definition site rather than on access
		(eager). Accessing x recalls the stored value without re-running the code
		(memoized).
	*/

		val x = {
			println("Computing X")
			math.random
		}                                 //> Computing X
                                                  //| x  : Double = 0.4334073719197604

		x // first access                 //> res0: Double = 0.4334073719197604

		x // second access                //> res1: Double = 0.4334073719197604


	/*
		By contrast, defs are lazy and not memoized. The code to compute y below is
		not run until we access it (lazy), and is re-run on every access (not memoized):
	*/
	
		def y = {
			println("Computing Y")
			math.random
		}                                 //> y: => Double

		y // first access                 //> Computing Y
                                                  //| res2: Double = 0.26221980015218505

		y // second access                //> Computing Y
                                                  //| res3: Double = 0.01750241326916835


	/*
		Last but not least, lazy vals are lazy and memoized. The code to compute
		z below is not run until we access it for the first time (lazy). The result is then
		cached and re-used on subsequent accesses (memoized):
	*/

		lazy val z = {
			println("Computing Z")
			math.random
		}                                 //> z: => Double = <lazy>

		z // first access                 //> Computing Z
                                                  //| res4: Double = 0.8395253272353052

		z // second access                //> res5: Double = 0.8395253272353052

	/*
		4.6.2 Eval’s Models of Evaluation
		Eval has three subtypes: Now, Later, and Always. We construct these with
		three constructor methods, which create instances of the three classes and
		return them typed as Eval:
	*/
	
		import cats.Eval
		
		val now = Eval.now(math.random + 1000)
                                                  //> now  : cats.Eval[Double] = Now(1000.7242783415551)

		val later = Eval.later(math.random + 2000)
                                                  //> later  : cats.Eval[Double] = cats.Later@69d9c55

		val always = Eval.always(math.random + 3000)
                                                  //> always  : cats.Eval[Double] = cats.Always@7ca48474

	/*
		We can extract the result of an Eval using its value method:
	*/
	
		now.value                         //> res6: Double = 1000.7242783415551

		later.value                       //> res7: Double = 2000.6793744664312

		always.value                      //> res8: Double = 3000.5348500629066


	/*
		Each type of Eval calculates its result using one of the evaluation models
		defined above. Eval.now captures a value right now. Its semantics are similar
		to a val—eager and memoized:
	*/
		
		val x1 = Eval.now {
			println("Computing X")
			math.random
		}                                 //> Computing X
                                                  //| x1  : cats.Eval[Double] = Now(0.01596124300494206)

		x1.value // first access          //> res9: Double = 0.01596124300494206

		x1.value // second access         //> res10: Double = 0.01596124300494206

	/*
		Eval.always captures a lazy computation, similar to a def:
	*/

		val y1 = Eval.always {
			println("Computing Y")
			math.random
		}                                 //> y1  : cats.Eval[Double] = cats.Always@59e84876

		y1.value // first access          //> Computing Y
                                                  //| res11: Double = 0.8383787745329692

		y1.value // second access         //> Computing Y
                                                  //| res12: Double = 0.4812701797788299
	
	/*
		Finally, Eval.later captures a lazy, memoized computation, similar to a lazy
		val:
	*/

		val z1 = Eval.later {
			println("Computing Z")
			math.random
		}                                 //> z1  : cats.Eval[Double] = cats.Later@39fb3ab6

		z1.value // first access          //> Computing Z
                                                  //| res13: Double = 0.792505615534007

		z1.value // second access         //> res14: Double = 0.792505615534007


	/*
		The three behaviours are summarized below:
		
		------------------------------------------------
		Scala      Cats     Properties
		------------------------------------------------
		val        Now      eager, memoized
		lazy val   Later    lazy, memoized
		def        Always   lazy, not memoized
		------------------------------------------------

		4.6.3 Eval as a Monad

		Like all monads, Eval's map and flatMap methods add computations to a
		chain. In this case, however, the chain is stored explicitly as a list of functions.
		The functions aren’t run until we call Eval's value method to request a result:
	*/
	
		val greeting = Eval.
			always { println("Step 1"); "Hello" }.
			map { str => println("Step 2"); s"$str world" }
                                                  //> greeting  : cats.Eval[String] = cats.Eval$$anon$9@2669b199

		greeting.value                    //> Step 1
                                                  //| Step 2
                                                  //| res15: String = Hello world

	
	/*
		Note that, while the semantics of the originating Eval instances are main106
		tained, mapping functions are always called lazily on demand (def semantics):
	*/

		val ans = for {
			a <- Eval.now { println("Calculating A"); 40 }
			b <- Eval.always { println("Calculating B"); 2 }
		} yield {
			println("Adding A and B")
			a + b
		}                                 //> Calculating A
                                                  //| ans  : cats.Eval[Int] = cats.Eval$$anon$9@7ac7a4e4

		ans.value // first access         //> Calculating B
                                                  //| Adding A and B
                                                  //| res16: Int = 42

		ans.value // second access        //> Calculating B
                                                  //| Adding A and B
                                                  //| res17: Int = 42

	/*
		Eval has a memoize method that allows us to memoize a chain of computa-
		tions. The result of the chain up to the call to memoize is cached, whereas
		calculations after the call retain their original semantics:
	*/
		val saying = Eval.
			always { println("Step 1"); "The cat" }.
			map { str => println("Step 2"); s"$str sat on" }.
			memoize.
			map { str => println("Step 3"); s"$str the mat" }
                                                  //> saying  : cats.Eval[String] = cats.Eval$$anon$9@36d4b5c

		saying.value // first access      //> Step 1
                                                  //| Step 2
                                                  //| Step 3
                                                  //| res18: String = The cat sat on the mat

		saying.value // second access     //> Step 3
                                                  //| res19: String = The cat sat on the mat

	/*
		4.6.4 Trampolining and Eval.defer
		One useful property of Eval is that its map and flatMap methods are trampolined.
		This means we can nest calls to map and flatMap arbitrarily without
		consuming stack frames. We call this property “stack safety”.

		For example, consider this function for calculating factorials:
			def factorial(n: BigInt): BigInt =
				if(n == 1) n else n * factorial(n - 1)

		It is relatively easy to make this method stack overflow:
		
		// factorial(50000)
		// java.lang.StackOverflowError
		// ...

		We can rewrite the method using Eval to make it stack safe:

		def factorial(n: BigInt): Eval[BigInt] =
			if(n == 1) {
				Eval.now(n)
			} else {
				factorial(n - 1).map(_ * n)
			}

		// factorial(50000).value
		// java.lang.StackOverflowError
		// ...
		
		Oops! That didn’t work—our stack still blew up! This is because we’re still making
		all the recursive calls to factorial before we start working with Eval's
		map method. We can work around this using Eval.defer, which takes an exis
		ting instance of Eval and defers its evaluation. The defer method is trampolined
		like map and flatMap, so we can use it as a quick way to make an
		existing operation stack safe:
	*/
		
		def factorial(n: BigInt): Eval[BigInt] =
			if(n == 1) {
				Eval.now(n)
			} else {
				Eval.defer(factorial(n - 1).map(_ * n))
			}                         //> factorial: (n: BigInt)cats.Eval[BigInt]
		
		factorial(50000).value            //> res20: BigInt = 33473205095971448369154760940714864779127732238104548077301
                                                  //| 003219901680221443656416973812310719169308798480438190208299893616384743066
                                                  //| 693742630572845363784038325756282123359987268244078235972356040853854441373
                                                  //| 383753568565536371168327405166076155165921406156075461294201790567479665498
                                                  //| 629242220022541553510718159801615476451810616674970217996537474972541139338
                                                  //| 191638823500630307644256874857271394651081909874909643486268589229807870031
                                                  //| 031008962861154553979911612940652327396971497211031261142860733793509687837
                                                  //| 355811830609551728906603833592532851635961730885279811957399495299450306354
                                                  //| 442478492641028990069559634883529900557676550929175475920788044807622562415
                                                  //| 165130459046318068517406766360012329556454065724225175473428183121029195715
                                                  //| 593787423641117194513838593038006413132976312508980623953869845352836267459
                                                  //| 097392518734779173869805487441821856484385034919643337438460714767001812780
                                                  //| 97686695715537229628555
                                                  //| Output exceeds cutoff limit.

  /*
		Eval is a useful tool to enforce stack safety when working on very large computa
		tions and data structures. However, we must bear in mind that trampolining
		is not free. It avoids consuming stack by creating a chain of function objects
		on the heap. There are still limits on how deeply we can nest computations,
		but they are bounded by the size of the heap rather than the stack.

		4.6.5 Exercise: Safer Folding using Eval

		The naive implementation of foldRight below is not stack safe. Make it so
		using Eval:

		def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
			as match {
				case head :: tail =>
					fn(head, foldRight(tail, acc)(fn))
				case Nil =>
					acc
			}
		See the solution
			----------------------- solution ------------------------------

		D.4 Safer Folding using Eval

		The easiest way to fix this is to introduce a helper method called
		foldRightEval. This is essentially our original method with every occurrence
		of B replaced with Eval[B], and a call to Eval.defer to protect the
		recursive call:
	*/

  import cats.Eval
  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }                                             //> foldRightEval: [A, B](as: List[A], acc: cats.Eval[B])(fn: (A, cats.Eval[B])
                                                  //|  => cats.Eval[B])cats.Eval[B]

  /*
		We can redefine foldRight simply in terms of foldRightEval and the resul
		ting method is stack safe:
	*/

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))

    }.value                                       //> foldRight: [A, B](as: List[A], acc: B)(fn: (A, B) => B)B
    
  foldRight((1 to 100000).toList, 0L)(_ + _)      //> res21: Long = 5000050000


	/*
	    ---------------------------------------------------------------

	*/

}
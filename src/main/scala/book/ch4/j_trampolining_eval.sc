package book.ch4

object j_trampolining_eval {
  /* 4.6.4 Trampolining and Eval.defer
	One useful property of Eval is that its map and flatMap methods are trampolined.
	This means we can nest calls to map and flatMap arbitrarily without
	consuming stack frames. We call this property “stack safety”.
	For example, consider this func􀦞on for calcula􀦞ng factorials:
	*/

  def factorial(n: BigInt): BigInt =
    if (n == 1) n else n * factorial(n - 1)       //> factorial: (n: BigInt)BigInt

  //  factorial(50000) // java.lang.StackOverflowError

  import cats.Eval
  def factorial2(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      factorial2(n - 1).map(_ * n)
    }                                             //> factorial2: (n: BigInt)cats.Eval[BigInt]

  // factorial2(50000).value // java.lang.StackOverflowError

  def factorial3(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial3(n - 1).map(_ * n))
    }                                             //> factorial3: (n: BigInt)cats.Eval[BigInt]
  factorial3(50000).value                         //> res0: BigInt = 3347320509597144836915476094071486477912773223810454807730100
                                                  //| 3219901680221443656416973812310719169308798480438190208299893616384743066693
                                                  //| 7426305728453637840383257562821233599872682440782359723560408538544413733837
                                                  //| 5356856553637116832740516607615516592140615607546129420179056747966549862924
                                                  //| 2220022541553510718159801615476451810616674970217996537474972541139338191638
                                                  //| 8235006303076442568748572713946510819098749096434862685892298078700310310089
                                                  //| 6286115455397991161294065232739697149721103126114286073379350968783735581183
                                                  //| 0609551728906603833592532851635961730885279811957399495299450306354442478492
                                                  //| 6410289900695596348835299005576765509291754759207880448076225624151651304590
                                                  //| 4631806851740676636001232955645406572422517547342818312102919571559378742364
                                                  //| 1117194513838593038006413132976312508980623953869845352836267459097392518734
                                                  //| 7791738698054874418218564843850349196433374384607147670018127809768669571553
                                                  //| 722962855502892722067813
                                                  //| Output exceeds cutoff limit.

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }                                             //> foldRight: [A, B](as: List[A], acc: B)(fn: (A, B) => B)B

  def foldRightEval[A, B](as: List[A], acc: B)(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        Eval.now(acc)
    }                                             //> foldRightEval: [A, B](as: List[A], acc: B)(fn: (A, cats.Eval[B]) => cats.Ev
                                                  //| al[B])cats.Eval[B]

	def foldRight1[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
		foldRightEval(as, acc) { (a, b) => b.map(fn(a, _))}.value
                                                  //> foldRight1: [A, B](as: List[A], acc: B)(fn: (A, B) => B)B

  foldRight1((1 to 100000).toList, 0L)(_ + _)     //> res1: Long = 5000050000
}
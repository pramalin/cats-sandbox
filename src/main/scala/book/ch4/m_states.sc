package book.ch4

object m_states {

  /*
		cats.data.State allows us to pass additional state around as part of a computa
		tion. We define State instances representing atomic state operations
		and thread them together using map and flatMap. In this way we can model
		mutable state in a purely func􀦞onal way, without using muta􀦞on.
	*/

  // 4.9.1 Creating and Unpacking State
  /*  Boiled down to their simplest form, instances of State[S, A] represent func-
			tions of type S => (S, A). S is the type of the state and A is the type of the
			result.
  */

  import cats.data.State

  val a = State[Int, String] { state => (state, s"The state is $state") }
                                                  //> a  : cats.data.State[Int,String] = cats.data.IndexedStateT@ba8a1dc

  /*
		We can “run” our monad by supplying an initial state. State provides three
		methods—run, runS, and runA—that return different combinations of state
		and result. Each method returns an instance of Eval, which State uses to
		maintain stack safety. We call the value method as usual to extract the actual
		result:
	*/

  // Get the state and the result:
  val (state, result) = a.run(10).value           //> state  : Int = 10
                                                  //| result  : String = The state is 10

  // Get the state, ignore the result:
  val state1 = a.runS(10).value                   //> state1  : Int = 10

  // Get the result, ignore the state:
  val result1 = a.runA(10).value                  //> result1  : String = The state is 10
  // result: String = The state is 10

  /*
		4.9.2 Composing and Transforming State
		As we’ve seen with Reader and Writer, the power of the State monad
		comes from combining instances. The map and flatMap methods thread the
		state from one instance to another. Each individual instance represents an
		atomic state transformation, and their combination represents a complete sequence
		of changes:
	*/

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }                                               //> step1  : cats.data.State[Int,String] = cats.data.IndexedStateT@27efef64

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }                                               //> step2  : cats.data.State[Int,String] = cats.data.IndexedStateT@47c62251

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)                                  //> both  : cats.data.IndexedStateT[cats.Eval,Int,Int,(String, String)] = cats.
                                                  //| data.IndexedStateT@1e88b3c
  val (state2, result2) = both.run(20).value      //> state2  : Int = 42
                                                  //| result2  : (String, String) = (Result of step1: 21,Result of step2: 42)


  /*
		The general model for using the State monad is to represent each step of a
		computation as an instance and compose the steps using the standard monad
		operators. Cats provides several convenience constructors for creating primi-
		tive steps:
		• get extracts the state as the result;
		• set updates the state and returns unit as the result;
		• pure ignores the state and returns a supplied result;
		• inspect extracts the state via a transformation function;
		• modify updates the state using an update function.
	*/

  val getDemo = State.get[Int]                    //> getDemo  : cats.data.State[Int,Int] = cats.data.IndexedStateT@4dfa3a9d
  getDemo.run(10).value                           //> res0: (Int, Int) = (10,10)
  val setDemo = State.set[Int](30)                //> setDemo  : cats.data.State[Int,Unit] = cats.data.IndexedStateT@464bee09
  setDemo.run(10).value                           //> res1: (Int, Unit) = (30,())

  val pureDemo = State.pure[Int, String]("Result")//> pureDemo  : cats.data.State[Int,String] = cats.data.IndexedStateT@13deb50e
                                                  //| 
  pureDemo.run(10).value                          //> res2: (Int, String) = (10,Result)

  val inspectDemo = State.inspect[Int, String](_ + "!")
                                                  //> inspectDemo  : cats.data.State[Int,String] = cats.data.IndexedStateT@3abbfa
                                                  //| 04
  inspectDemo.run(10).value                       //> res3: (Int, String) = (10,10!)
  val modifyDemo = State.modify[Int](_ + 1)       //> modifyDemo  : cats.data.State[Int,Unit] = cats.data.IndexedStateT@553f17c
  modifyDemo.run(10).value                        //> res4: (Int, Unit) = (11,())

  /*
		We can assemble these building blocks using a for comprehension. We typically
		ignore the result of intermediate stages that only represent transforma-
		tions on the state:
	*/

  import State._
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)                               //> program  : cats.data.State[Int,(Int, Int, Int)] = cats.data.IndexedStateT@2
                                                  //| 71053e1

  val (state3, result3) = program.run(1).value    //> state3  : Int = 3
                                                  //| result3  : (Int, Int, Int) = (1,2,3000)

}
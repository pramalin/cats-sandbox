package book.ch4

object g_eval_monad {

	// Note: The worksheets doesn't work properly with the Evals. Need to run the snippets in REPL.

  // 4.6.1 Eager, Lazy, Memoized, Oh My!
  /*
  What do these terms mean?

	Eager computa􀦞ons happen immediately whereas lazy computa􀦞ons happen
	on access. Memoized computa􀦞ons are run once on first access, a􀁛er which
	the results are cached.

	For example, Scala vals are eager and memoized. We can see this using a
	computa􀦞on with a visible side-effect. In the following example, the code
	to compute the value of x happens at the definition site rather than on access
	(eager). Accessing x recalls the stored value without re-running the code
	(memoized).
	*/

  val x = {
    println("Computing X")
    math.random
  }

  x // first access
  x // second access

  /*
	By contrast, defs are lazy and not memoized. The code to compute y below is
	not run until we access it (lazy), and is re-run on every access (not memoized):
	*/
  def y = {
    println("Computing Y")
    math.random
  }

  //  y // first access

  //  y // second access
  /*
	Last but not least, lazy vals are lazy and memoized. The code to compute
	z below is not run until we access it for the first time (lazy). The result is then
	cached and re-used on subsequent accesses (memoized):
	*/

  lazy val z = {
    println("Computing Z")
    math.random
  }

  z // first access
  z // second access

}
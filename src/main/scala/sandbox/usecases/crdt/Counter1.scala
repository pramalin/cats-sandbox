package sandbox.usecases.crdt

/*
 		K.1 GCounter Implementation

		Hopefully the description above was clear enough that you can get to an implementa
		tion like the one below.			
*/

object Counter1 {
  final case class GCounter1(counters: Map[String, Int]) {

    def increment(machine: String, amount: Int) = {
      val value = amount + counters.getOrElse(machine, 0)
      GCounter1(counters + (machine -> value))
    }

    def merge(that: GCounter1): GCounter1 =
      GCounter1(that.counters ++ this.counters.map {
        case (k, v) =>
          k -> (v max that.counters.getOrElse(k, 0))
      })

    def total: Int =
      counters.values.sum
  }
}
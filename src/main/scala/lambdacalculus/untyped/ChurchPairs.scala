package lambdacalculus.untyped

/**
  * Created by k_higuchi on 2017/07/16.
  */
object ChurchPairs {

  type ChurchPair = Any => Any => (Any => Any => Any) => Any
  type PartiallyAppliedChurchPair = (Any => Any => Any) => Any

  val Pair: ChurchPair = x => y => p => p(x)(y)
  val Left: PartiallyAppliedChurchPair => PartiallyAppliedChurchPair = pp => pp(x => y => x).asInstanceOf[PartiallyAppliedChurchPair]
  val Right: PartiallyAppliedChurchPair => PartiallyAppliedChurchPair = pp => pp(x => y => y).asInstanceOf[PartiallyAppliedChurchPair]

}

package lambdacalculus.untyped

import lambdacalculus.untyped.ChurchNumerals._
import lambdacalculus.untyped.ChurchBooleans._
import lambdacalculus.untyped.Predicates._
import lambdacalculus.untyped.ChurchPairs._
import lambdacalculus.untyped.FixedPointOperators._

/**
  * Created by k_higuchi on 2017/07/16.
  */
object Operators {

  val Increment: ChurchNumeral => ChurchNumeral = n => f => x => f(n(f)(x))

  val Slide: PartiallyAppliedChurchPair => PartiallyAppliedChurchPair =
    pp => Pair(Right(pp).asInstanceOf[ChurchNumeral])(Increment(Right(pp).asInstanceOf[ChurchNumeral]))

  val Decrement: ChurchNumeral => ChurchNumeral = n => Left(
    n(Slide.asInstanceOf[Any => Any])(Pair(Zero)(Zero)).asInstanceOf[PartiallyAppliedChurchPair]
  ).asInstanceOf[ChurchNumeral]

  val Add: ChurchNumeral => ChurchNumeral => ChurchNumeral = m => n => n(Increment.asInstanceOf[Any => Any])(m).asInstanceOf[ChurchNumeral]
  val Subtract: ChurchNumeral => ChurchNumeral => ChurchNumeral = m => n => n(Decrement.asInstanceOf[Any => Any])(m).asInstanceOf[ChurchNumeral]
  val Multiply: ChurchNumeral => ChurchNumeral => ChurchNumeral = m => n => n(Add(m).asInstanceOf[Any => Any])(Zero).asInstanceOf[ChurchNumeral]
  val Power: ChurchNumeral => ChurchNumeral => ChurchNumeral = m => n => n(Multiply(m).asInstanceOf[Any => Any])(One).asInstanceOf[ChurchNumeral]

  val Mod = Z { (f: ChurchNumeral => ChurchNumeral => Any) =>
    (m: ChurchNumeral) => (n: ChurchNumeral) =>
      If (IsLessOrEqual(n)(m)) {
        (x: Any) => f(Subtract(m)(n))(n).asInstanceOf[Any => Any](x)
      }{ m }
  }

}

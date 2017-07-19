package lambdacalculus.untyped

import lambdacalculus.untyped.ChurchNumerals._
import lambdacalculus.untyped.ChurchBooleans._
import lambdacalculus.untyped.Predicates._
import lambdacalculus.untyped.ChurchPairs._
import lambdacalculus.untyped.FixedPointOperations._

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


  val UnsafeFact: Any => Any = Y (
    (f: (Any => Any)) =>
      (n: Any) =>
        If(IsLessOrEqual(n.asInstanceOf[ChurchNumeral])(Zero))(One)(
          Multiply(n.asInstanceOf[ChurchNumeral])(
            f(Subtract(n.asInstanceOf[ChurchNumeral])(One)).asInstanceOf[ChurchNumeral]
          )
        )
  )

  val UnsafeMod: Any => Any = Y {
    (f: (Any => Any)) =>
      (m: Any) => (n: Any) =>
        If(IsLessOrEqual(n.asInstanceOf[ChurchNumeral])(m.asInstanceOf[ChurchNumeral]))(
          f(
            Subtract(m.asInstanceOf[ChurchNumeral])(n.asInstanceOf[ChurchNumeral])
          ).asInstanceOf[Any => Any](n)
        )(m)
  }

//  val Mod = Z { (f: Any => Any) => (m: Any) => (n: Any) =>
//    If(IsLessOrEqual(n.asInstanceOf[ChurchNumeral])(m.asInstanceOf[ChurchNumeral])) (
//      (x: Any) => f.asInstanceOf[Any => Any => Any => Any] (
//        Subtract(m.asInstanceOf[ChurchNumeral])(n.asInstanceOf[ChurchNumeral])
//      )(n)(x)
//    )(m)
//  }

  val Mod: Any => Any = Z { (f: Any) =>
    (m: Any) => (n: Any) =>
      If(IsLessOrEqual(n.asInstanceOf[ChurchNumeral])(m.asInstanceOf[ChurchNumeral])) {
        (x: Any) => {
          f.asInstanceOf[Any => Any => Any => Any](Subtract(m.asInstanceOf[ChurchNumeral])(n.asInstanceOf[ChurchNumeral]))(n)(x)
        }
      }{
        m
      }
  }

}

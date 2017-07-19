package lambdacalculus.untyped

import ChurchNumerals._
import ChurchBooleans._
import Predicates._
import ChurchPairs._
import Operators._

/**
  * Created by k_higuchi on 2017/07/16.
  */
object UTLambdaCalculusSimulate extends App {

  println(toInt(Five))
  println(toInt(Fifteen))
  println(toInt(Hundred))

  println(toBoolean(True))
  println(toBoolean(False))

  println(If(True)("happy")("sad"))
  println(If(False)("happy")("sad"))

  println(toBoolean(IsZero(Zero)))
  println(toBoolean(IsZero(One)))

  println(toInt(Left(Pair(One)(Zero)).asInstanceOf[ChurchNumeral]))
  println(toInt(Right(Pair(One)(Zero)).asInstanceOf[ChurchNumeral]))

  println(toInt(Left(Slide(Pair(Zero)(One))).asInstanceOf[ChurchNumeral]))
  println(toInt(Right(Slide(Pair(Zero)(One))).asInstanceOf[ChurchNumeral]))

  println(toInt(Increment(Five)))
  println(toInt(Increment(Hundred)))

  println(toInt(Decrement(Five)))
  println(toInt(Decrement(Hundred)))

  println(toInt(Add(One)(Five)))
  println(toInt(Subtract(Hundred)(Five)))
  println(toInt(Multiply(Fifteen)(Hundred)))
  println(toInt(Power(Five)(Five)))

  println(toBoolean(IsLessOrEqual(Five)(Fifteen)))
  println(toBoolean(IsLessOrEqual(Fifteen)(Fifteen)))
  println(toBoolean(IsLessOrEqual(Fifteen)(Five)))

  println(toInt(Mod.asInstanceOf[Any => Any => Any](Fifteen)(Five).asInstanceOf[ChurchNumeral]))

}

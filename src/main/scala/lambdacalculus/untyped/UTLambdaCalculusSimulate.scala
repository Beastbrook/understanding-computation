package lambdacalculus.untyped

import ChurchNumerals._
import ChurchBooleans._
import Predicates._
import ChurchPairs._
import Operators._
import ListEncodings._

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

  println(toInt(Mod(Fifteen)(Five).asInstanceOf[ChurchNumeral]))
  println(toInt(Mod(Five)(Two).asInstanceOf[ChurchNumeral]))
  println(toInt(Mod(Five)(Three).asInstanceOf[ChurchNumeral]))

  val myList: PartiallyAppliedChurchPair =
    UnShift(
      UnShift(
        UnShift(Empty)(Three)
      )(Two)
    )(One)

  println(toList(myList).map((n: Any) => toInt(n.asInstanceOf[ChurchNumeral])))
  println(toInt(First(myList).asInstanceOf[ChurchNumeral]))
  println(toInt(First(Rest(myList)).asInstanceOf[ChurchNumeral]))
  println(toInt(First(Rest(Rest(myList))).asInstanceOf[ChurchNumeral]))
  println(toBoolean(IsEmpty(myList).asInstanceOf[ChurchBoolean]))
  println(toBoolean(IsEmpty(Empty).asInstanceOf[ChurchBoolean]))

  val myRange = Range(One)(Five).asInstanceOf[PartiallyAppliedChurchPair]
  println(toList(myRange).map((n: Any) => toInt(n.asInstanceOf[ChurchNumeral])))

  val foldedList = Fold(myRange)(Zero)(Add)

  println(toInt(foldedList.asInstanceOf[ChurchNumeral]))

  val mappedRange = Map(myRange)(Increment.asInstanceOf[Any => Any])

  println(toList(mappedRange.asInstanceOf[PartiallyAppliedChurchPair])
    .map((n: Any) => toInt(n.asInstanceOf[ChurchNumeral]))
  )

}

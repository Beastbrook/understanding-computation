package lambdacalculus.untyped

import ChurchNumerals._
import ChurchBooleans._
import Predicates._
import ChurchPairs._
import Operators._
import FixedPointOperators._

import scala.collection.mutable

/**
  * Created by k_higuchi on 2017/07/20.
  */
object ListEncodings {

  val Empty: PartiallyAppliedChurchPair = Pair(True)(True)
  val UnShift: Any => Any => PartiallyAppliedChurchPair = t => h => Pair(False)(Pair(h)(t))
  val IsEmpty: PartiallyAppliedChurchPair => PartiallyAppliedChurchPair = Left
  val First: PartiallyAppliedChurchPair => PartiallyAppliedChurchPair = l => Left(Right(l))
  val Rest: PartiallyAppliedChurchPair => PartiallyAppliedChurchPair = l => Right(Right(l))

  val Range = Z { (f: ChurchNumeral => ChurchNumeral => Any) =>
    (m: ChurchNumeral) => (n: ChurchNumeral) =>
      If(IsLessOrEqual(m)(n)) {
        (x: Any) => UnShift(f(Increment(m))(n))(m).asInstanceOf[Any => Any](x)
      } { Empty }
  }

  val Fold = Z { (f: PartiallyAppliedChurchPair => Any => Any => Any) =>
    (l: PartiallyAppliedChurchPair) => (x: Any) => (g: Any) =>
    If(IsEmpty(l).asInstanceOf[ChurchBoolean]) {
      x
    } {
      (y: Any) =>
        g.asInstanceOf[Any => Any => Any](f(Rest(l))(x)(g))(First(l)).asInstanceOf[Any => Any](y)
    }
  }

  val Map: PartiallyAppliedChurchPair => (Any => Any) => Any = k => f =>
    Fold(k)(Empty) {
      (l: PartiallyAppliedChurchPair) => (x: Any) => UnShift(l)(f(x))
    }

  def toList(pap: PartiallyAppliedChurchPair): List[Any] = {
    val result = mutable.ListBuffer[Any]()
    var source = pap
    while(!toBoolean(IsEmpty(source).asInstanceOf[ChurchBoolean])) {
      result += First(source)
      source = Rest(source)
    }
    result.toList
  }

}

package lambdacalculus.untyped

import ChurchNumerals.ChurchNumeral
import ChurchBooleans.{ChurchBoolean, True, False}
import lambdacalculus.untyped.Operators.Subtract

/**
  * Created by k_higuchi on 2017/07/16.
  */
object Predicates {

  val IsZero: ChurchNumeral => ChurchBoolean = n => n((x: Any) => False)(True).asInstanceOf[ChurchBoolean]

  val IsLessOrEqual: ChurchNumeral => ChurchNumeral => ChurchBoolean =
    m => n => IsZero(Subtract(m)(n))

}

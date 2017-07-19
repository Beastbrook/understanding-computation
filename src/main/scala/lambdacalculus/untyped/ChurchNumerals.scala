package lambdacalculus.untyped

/**
  * Created by k_higuchi on 2017/07/16.
  */
object ChurchNumerals {

  type ChurchNumeral = (Any => Any) => (Any => Any)

  val Zero: ChurchNumeral = f => x => x
  val One: ChurchNumeral = f => x => f(x)
  val Two: ChurchNumeral = f => x => f(f(x))
  val Three: ChurchNumeral = f => x => f(f(f(x)))
  val Five: ChurchNumeral = f => x => f(f(f(f(f(x)))))
  val Fifteen: ChurchNumeral = f => x => f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(x)))))))))))))))
  val Hundred: ChurchNumeral = f => x => f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

  def toInt(func: ChurchNumeral): Int = func(n => n.asInstanceOf[Int] + 1)(0).asInstanceOf[Int]

}

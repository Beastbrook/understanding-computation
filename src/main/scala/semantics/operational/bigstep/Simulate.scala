package semantics.operational.bigstep

/**
  * Created by k_higuchi on 2017/07/02.
  */
object Simulate {

  def main(args: Array[String]): Unit = {

    val lessThanExpr = LessThan(Add(Variable("x"), Number(2)), Variable("y"))
    val lessThanResult = lessThanExpr.evaluate(
      Environment("x" -> Number(2), "y" -> Number(5))
    )
    println(lessThanExpr)
    println(lessThanResult)

    val sequenceStmt = Sequence(
      Assign("x", Add(Number(1), Number(1))),
      Assign("y", Add(Variable("x"), Number(3)))
    )
    println(sequenceStmt)
    println(sequenceStmt.evaluate(Environment()))

    val whileLoopStmt = While(
      LessThan(Variable("x"), Number(5)),
      Assign("x", Add(Variable("x"), Number(1)))
    )
    println(whileLoopStmt)
    println(whileLoopStmt.evaluate(Environment("x" -> Number(1))))

  }

}

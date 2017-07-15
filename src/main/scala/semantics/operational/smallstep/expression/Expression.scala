package semantics.operational.smallstep.expression


import semantics.operational.smallstep.machine.Environment

import scala.annotation.tailrec

/**
  * Created by k_higuchi on 2017/07/02.
  */
sealed trait Expression

object Expression {

  def reduceAll(expression: Expression, printEachStep: Boolean = false)
               (implicit environment: Environment): IrreducibleExpression = {
    @tailrec
    def loop(expr: Expression): IrreducibleExpression = {
      if(printEachStep) println(expr)
      expr match {
        case irep: IrreducibleExpression => irep
        case rdep: ReducibleExpression => loop(rdep.reduce)
      }
    }
    loop(expression)
  }

}

trait IrreducibleExpression extends Expression

trait ReducibleExpression extends Expression {
  def reduce(implicit environment: Environment): Expression
}
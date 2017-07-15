package semantics.operational.smallstep.statement

import semantics.operational.smallstep.machine.Environment

import scala.annotation.tailrec

/**
  * Created by k_higuchi on 2017/07/02.
  */
sealed trait Statement

object Statement {

  def reduceAll(statement: Statement, printEachStep: Boolean = false)
               (implicit environment: Environment): StatementReduceResult = {
    @tailrec
    def loop(stmt: Statement, env: Environment): StatementReduceResult = {
      if(printEachStep) println(stmt)
      stmt match {
        case _: IrreducibleStatement =>
          StatementReduceResult(stmt, env)
        case rdst: ReducibleStatement =>
          val result = rdst.reduce(env) //ここで明示的にenvを渡さないと古い方が供給される
          loop(result.statement, result.environment)
      }
    }
    loop(statement, environment)
  }

}

sealed trait IrreducibleStatement extends Statement

case class DoNothing() extends IrreducibleStatement {
  override def toString: String = "do-nothing"
}

trait ReducibleStatement extends Statement {
  def reduce(implicit environment: Environment): StatementReduceResult
}

case class StatementReduceResult(statement: Statement, environment: Environment)
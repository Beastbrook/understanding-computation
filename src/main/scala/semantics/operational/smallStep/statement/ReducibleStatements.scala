package semantics.operational.smallStep.statement

import semantics.operational.smallStep.expression.{Bool, Expression, IrreducibleExpression, ReducibleExpression}
import semantics.operational.smallStep.machine.Environment

/**
  * Created by k_higuchi on 2017/07/02.
  */
case class Assign(name: String, expression: Expression) extends ReducibleStatement {

  override def toString: String = s"${this.name} := ${this.expression}"

  override def reduce(implicit environment: Environment): StatementReduceResult = expression match {
    case irst: IrreducibleExpression =>
      StatementReduceResult(DoNothing(), environment.write(name, irst))
    case rdst: ReducibleExpression =>
      StatementReduceResult(Assign(name, rdst.reduce), environment)
  }

}

case class If(
  condition: Expression,
  consequence: Statement,
  alternative: Statement
) extends ReducibleStatement {

  override def toString: String =
    s"if(${this.condition}) {${this.consequence}} else {${this.alternative}}"

  override def reduce(implicit environment: Environment): StatementReduceResult = condition match {
    case irep: IrreducibleExpression =>
      val bool = irep.asInstanceOf[Bool].value
      if (bool)
        StatementReduceResult(consequence, environment)
      else
        StatementReduceResult(alternative, environment)
    case rdep: ReducibleExpression =>
      val result = If(rdep.reduce, consequence, alternative)
      StatementReduceResult(result, environment)
  }

}

case class Sequence(fist: Statement, second: Statement) extends ReducibleStatement {

  override def toString: String = s"${this.fist}; ${this.second}"

  override def reduce(implicit environment: Environment): StatementReduceResult = fist match {
    case _: IrreducibleStatement=>
      StatementReduceResult(second, environment)
    case rdst: ReducibleStatement =>
      val result = rdst.reduce
      StatementReduceResult(Sequence(result.statement, second), result.environment)
  }

}

case class While(condition: Expression, body: Statement) extends ReducibleStatement {

  override def toString: String = s"while(${this.condition}) {${this.body}}"

  override def reduce(implicit environment: Environment): StatementReduceResult =
    StatementReduceResult(
      If(condition, Sequence(body, this), DoNothing()),
      environment
    )

}


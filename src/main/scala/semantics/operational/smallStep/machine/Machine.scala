package semantics.operational.smallStep.machine

import semantics.operational.smallStep.expression.{Expression, IrreducibleExpression, ReducibleExpression}
import semantics.operational.smallStep.statement.{IrreducibleStatement, ReducibleStatement, Statement}

/**
  * Created by k_higuchi on 2017/07/02.
  */
trait Machine

case class MachineE(expression: Expression, environment: Environment) extends Machine {

  private implicit val env = environment

  def step: MachineE = expression match {
    case _: IrreducibleExpression => this
    case expr: ReducibleExpression => MachineE(expr.reduce, environment)
  }

  def run(printEachStep: Boolean = false): MachineE = {
    val result = Expression.reduceAll(
      expression = expression,
      printEachStep = printEachStep
    )
    MachineE(result, environment)
  }

}

case class MachineS(statement: Statement, environment: Environment) extends Machine {

  private implicit val env = environment

  def step: MachineS = statement match {
    case _: IrreducibleStatement => this
    case rdst: ReducibleStatement =>
      val result = rdst.reduce
      MachineS(result.statement, result.environment)
  }

  def run(printEachStep: Boolean = false): MachineS = {
    val result = Statement.reduceAll(
      statement = statement,
      printEachStep = printEachStep
    )
    MachineS(result.statement, result.environment)
  }

}

case class Environment(private val initValues: (String, IrreducibleExpression)*) {

  type VariableName = String
  type EnvMap = Map[VariableName, IrreducibleExpression]

  private lazy val value: EnvMap = Map(initValues:_*)

  def read(name: VariableName): IrreducibleExpression = this.value(name)

  def write(name: VariableName, irreducibleExpression: IrreducibleExpression): Environment = {
    val newValue = this.value + (name -> irreducibleExpression)
    Environment(newValue.toSeq:_*)
  }

}
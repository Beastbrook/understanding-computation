package semantics.operational.smallstep

import semantics.operational.smallstep.expression._
import semantics.operational.smallstep.machine.{Environment, MachineE, MachineS}
import semantics.operational.smallstep.statement._

/**
  * Created by k_higuchi on 2017/07/02.
  */
object Simulation {

  def main(args: Array[String]): Unit = {

    val addMultiExpr = Add(
      Multiply(Number(1), Number(2)),
      Multiply(Number(3), Number(4))
    )
    MachineE(addMultiExpr, Environment()).run(printEachStep = true)

    val lessThanExpr = LessThan(Number(5), Add(Number(2), Number(2)))
    MachineE(lessThanExpr, Environment()).run(true)

    val variableReduceExpr = Add(Variable("x"), Variable("y"))
    val env1 = Environment("x" -> Number(3), "y" -> Number(4))
    MachineE(variableReduceExpr, env1).run(true)

    val variableAssignStmt = Assign("x", Add(Variable("x"), Number(1)))
    MachineS(variableAssignStmt, Environment("x" -> Number(2))).run(true)

    val ifStmt = If(
      Variable("x"),
      Assign("y", Number(1)),
      Assign("y", Number(2))
    )
    MachineS(ifStmt, Environment("x" -> Bool(true))).run(true)

    val IfWithNoElseStmt = If(
      Variable("x"),
      Assign("y", Number(1)),
      DoNothing()
    )
    MachineS(IfWithNoElseStmt, Environment("x" -> Bool(false))).run(true)

    val sequenceStmt = Sequence(
      Assign("x", Add(Number(1), Number(1))),
      Assign("y", Add(Variable("x"), Number(1)))
    )
    MachineS(sequenceStmt, Environment()).run(true)

    val whileLoopStmt = While(
      LessThan(Variable("x"), Number(5)),
      Assign("x", Multiply(Variable("x"), Number(3)))
    )
    MachineS(whileLoopStmt, Environment("x" -> Number(1))).run(true)

  }

}


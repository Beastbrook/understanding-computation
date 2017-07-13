package semantics.operational.smallStep.expression

import semantics.operational.smallStep.machine.Environment


/**
  * Created by k_higuchi on 2017/07/02.
  */
case class Add(left: Expression, right: Expression) extends ReducibleExpression {

  override def toString: String = s"${this.left} + ${this.right}"

  override def reduce(implicit environment: Environment): Expression = (left, right) match {
    case (l: IrreducibleExpression, r: IrreducibleExpression) =>
      Number(l.asInstanceOf[Number].value + r.asInstanceOf[Number].value)
    case (l: ReducibleExpression, _) =>
      Add(l.reduce, right)
    case (_, r: ReducibleExpression) =>
      Add(left, r.reduce)
  }

}

case class Multiply(left: Expression, right: Expression) extends ReducibleExpression {

  override def toString: String = s"${this.left} * ${this.right}"

  override def reduce(implicit environment: Environment): Expression = (left, right) match {
    case (l: IrreducibleExpression, r: IrreducibleExpression) =>
      Number(l.asInstanceOf[Number].value * r.asInstanceOf[Number].value)
    case (l: ReducibleExpression, _) =>
      Multiply(l.reduce, right)
    case (_, r: ReducibleExpression) =>
      Multiply(left, r.reduce)
  }

}


case class LessThan(left: Expression, right: Expression) extends ReducibleExpression {

  override def toString: String = s"${this.left} < ${this.right}"

  override def reduce(implicit environment: Environment): Expression = (left, right) match {
    case (l: IrreducibleExpression, r: IrreducibleExpression) =>
      Bool(l.asInstanceOf[Number].value < r.asInstanceOf[Number].value)
    case (l: ReducibleExpression, _) =>
      LessThan(l.reduce, right)
    case (_, r: ReducibleExpression) =>
      LessThan(left, r.reduce)
  }

}

case class Variable(name: String) extends ReducibleExpression {

  override def toString: String = name

  override def reduce(implicit environment: Environment): Expression = environment.read(name)

}
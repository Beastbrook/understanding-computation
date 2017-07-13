package semantics.operational.bigStep

/**
  * Created by k_higuchi on 2017/07/02.
  */
trait Expression {
  def evaluate(implicit environment: Environment): Expression
}

case class Number(value: Int) extends Expression {
  override def toString: String = this.value.toString
  override def evaluate(implicit environment: Environment): Expression = this
}

case class Bool(value: Boolean) extends Expression {
  override def toString: String = this.value.toString
  override def evaluate(implicit environment: Environment): Expression = this
}

case class Variable(name: String) extends Expression {
  override def toString: String = this.name
  override def evaluate(implicit environment: Environment): Expression =
    environment.read(name)
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def toString: String = s"${this.left} + ${this.right}"
  override def evaluate(implicit environment: Environment): Expression =
    Number(
      left.evaluate.asInstanceOf[Number].value +
        right.evaluate.asInstanceOf[Number].value)
}

case class Multiply(left: Expression, right: Expression) extends Expression {
  override def toString: String = s"${this.left} * ${this.right}"
  override def evaluate(implicit environment: Environment): Expression =
    Number(
      left.evaluate.asInstanceOf[Number].value *
        right.evaluate.asInstanceOf[Number].value)
}

case class LessThan(left: Expression, right: Expression) extends Expression {
  override def toString: String = s"${this.left} < ${this.right}"
  override def evaluate(implicit environment: Environment): Expression =
    Bool(
      left.evaluate.asInstanceOf[Number].value <
        right.evaluate.asInstanceOf[Number].value)
}
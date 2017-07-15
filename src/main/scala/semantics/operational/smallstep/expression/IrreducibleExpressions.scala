package semantics.operational.smallstep.expression

/**
  * Created by k_higuchi on 2017/07/02.
  */
case class Number(value: Int) extends IrreducibleExpression {
  override def toString: String = this.value.toString
}

case class Bool(value: Boolean) extends IrreducibleExpression {
  override def toString: String = this.value.toString
}
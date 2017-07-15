package semantics.operational.bigstep

/**
  * Created by k_higuchi on 2017/07/02.
  */
trait Statement {
  def evaluate(implicit environment: Environment): Environment
}

case class Assign(name: String, expression: Expression) extends Statement {
  override def toString: String = s"${this.name} := ${this.expression}"
  override def evaluate(implicit environment: Environment): Environment =
    environment.write(name, expression.evaluate)
}

case class DoNothing() extends Statement {
  override def toString: String = "do-nothing"
  override def evaluate(implicit environment: Environment): Environment = environment
}

case class If(
  condition: Expression,
  consequence: Statement,
  alternative: Statement
) extends Statement {
  override def toString: String =
    s"if(${this.condition}) {${this.consequence}} else {${this.alternative}}"
  override def evaluate(implicit environment: Environment): Environment = {
    val cond = condition.evaluate.asInstanceOf[Bool].value
    if(cond) consequence.evaluate else alternative.evaluate
  }
}

case class Sequence(first: Statement, second: Statement) extends Statement {
  override def toString: String = s"${this.first}; ${this.second}"
  override def evaluate(implicit environment: Environment): Environment =
    second.evaluate(first.evaluate)
}

case class While(condition: Expression, body: Statement) extends Statement {
  override def toString: String = s"while(${this.condition}) {${this.body}}"
  override def evaluate(implicit environment: Environment): Environment = {
    val cond = condition.evaluate.asInstanceOf[Bool].value
    if(cond) this.evaluate(body.evaluate) else environment
  }
}
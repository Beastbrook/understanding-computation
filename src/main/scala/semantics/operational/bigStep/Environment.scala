package semantics.operational.bigStep

/**
  * Created by k_higuchi on 2017/07/02.
  */
case class Environment(private val initValues: (String, Expression)*) {

  type VariableName = String
  type EnvMap = Map[VariableName, Expression]

  private lazy val value: EnvMap = Map(initValues:_*)

  override def toString: VariableName = s"Environment(${this.value.mkString(", ")})"

  def read(name: VariableName): Expression = this.value(name)

  def write(name: VariableName, expression: Expression): Environment = {
    val newValue = this.value + (name -> expression)
    Environment(newValue.toSeq:_*)
  }

}
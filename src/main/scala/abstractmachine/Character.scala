package abstractmachine



/**
  * Created by k_higuchi on 2017/07/09.
  */
sealed trait Character
case class NonEmptyChar(value: Char) extends Character {
  override def toString: String = this.value.toString
}
case object EmptyChar extends Character {
  override def toString: String = s"ε"
}

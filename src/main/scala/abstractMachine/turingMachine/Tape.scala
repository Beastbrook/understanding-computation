package abstractMachine.turingMachine

import abstractMachine.{Character, NonEmptyChar}

/**
  * Created by k_higuchi on 2017/07/15.
  */
class Tape private(
  val left: Vector[Character],
  val middle: Character,
  val right: Vector[Character],
  val blank: Character
) {

  def write(character: Character): Tape = this.copy(middle = character)

  private def toLeft: Tape =
    if(left.isEmpty) this
    else Tape(left.init, left.last, middle +: right, this.blank)

  private def toRight: Tape = Tape(left :+ middle, right.head, right.tail, this.blank)

  def move(direction: HeadMoveDirection): Tape = direction match {
    case Left => this.toLeft
    case Right => this.toRight
  }

  def copy(
    left: Vector[Character] = this.left,
    middle: Character = this.middle,
    right: Vector[Character] = this.right,
    blank: Character = this.blank
  ): Tape = Tape(left = left, middle = middle, right = right, blank = blank)

  override def toString: String =
    s"Tape[${this.left.mkString}(${this.middle})${this.right.mkString}]"

}

object Tape {

  def apply(
    left: Seq[Character] = Seq.empty,
    middle: Character,
    right: Seq[Character] = Seq.empty,
    blank: Character = NonEmptyChar('_')
  ): Tape = {
    val nonEmptyLeft = if(left.isEmpty) Vector(blank) else left.toVector
    val nonEmptyRight = if(right.isEmpty) Vector(blank) else right.toVector
    new Tape(nonEmptyLeft, middle, nonEmptyRight, blank)
  }

}

package abstractmachine.automata.pushdown

import abstractmachine.Character

/**
  * Created by k_higuchi on 2017/07/09.
  */
case class PDAStack(values: List[Character]) {

  def top: Character = values.head

  def push(character: Character): PDAStack = PDAStack(character +: values)

  def push(characters: Seq[Character]): PDAStack =
    characters.foldRight(this)((char, stack) => stack.push(char))

  def pop: PDAStackPopResult = PDAStackPopResult(values.head, PDAStack(values.tail))

  def isEmpty: Boolean = values.isEmpty

  def nonEmpty: Boolean = values.nonEmpty

  override def toString: String =
    if(values.isEmpty) "Stack(empty)"
    else s"Stack(Top(${values.head}) : Rest(${values.tail.mkString(", ")}))"

}

case class PDAStackPopResult(popCharacter: Character, stack: PDAStack)

object PDAStack {
  def apply(values: Character*): PDAStack = PDAStack(values.toList)
}
package abstractMachine.automata.finite

import abstractMachine.automata.{State, Character}

/**
  * Created by k_higuchi on 2017/07/08.
  */
case class FiniteAutomataRule(state: State, character: Character, nextState: State) {

  def canApplyTo(state: State, character: Character): Boolean =
    this.state == state && this.character == character

  override def toString: String =
    s"FARule ${this.state} --${this.character}-->${this.nextState}"

}
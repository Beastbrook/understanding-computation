package abstractMachine.automata.finite.deterministic

import abstractMachine.automata.{Character, NonEmptyChar, State}

/**
  * Created by k_higuchi on 2017/07/08.
  */
case class DeterministicFiniteAutomata(
  currentState: State = State(1),
  acceptStates: Set[State],
  rulebook: DFARulebook
) {

  val isAcceptState: Boolean = acceptStates.contains(currentState)

  def read(character: Character): DeterministicFiniteAutomata = {
    val nextState = this.rulebook.nextState(this.currentState, character)
    this.copy(currentState = nextState)
  }

  def read(string: String): DeterministicFiniteAutomata =
    string.foldLeft(this)( (dfa, char) => dfa.read(NonEmptyChar(char)) )

}

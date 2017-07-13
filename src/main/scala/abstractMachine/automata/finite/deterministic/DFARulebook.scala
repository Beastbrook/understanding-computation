package abstractMachine.automata.finite.deterministic

import abstractMachine.automata.{State, Character}
import abstractMachine.automata.finite.FiniteAutomataRule

/**
  * Created by k_higuchi on 2017/07/08.
  */
case class DFARulebook(rules: Set[FiniteAutomataRule]) {

  def nextState(state: State, character: Character): State =
    rules
      .find(_.canApplyTo(state, character))
      .map(_.nextState)
      .get // DFAは可能な入力に対して少なくとも一つのルールを持つ

}

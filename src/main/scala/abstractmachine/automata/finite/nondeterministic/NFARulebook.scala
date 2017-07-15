package abstractmachine.automata.finite.nondeterministic

import abstractmachine.{Character, State}
import abstractmachine.automata.finite.FiniteAutomataRule

/**
  * Created by k_higuchi on 2017/07/08.
  */
case class NFARulebook(rules: Set[FiniteAutomataRule]) {

  def nextStates(states: Set[State], character: Character): Set[State] =
    for {
      rule <- rules
      state <- states
      if rule.canApplyTo(state, character)
    } yield rule.nextState

}


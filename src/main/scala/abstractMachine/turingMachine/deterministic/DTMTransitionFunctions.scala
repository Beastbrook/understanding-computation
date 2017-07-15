package abstractMachine.turingMachine.deterministic

import abstractMachine.turingMachine.{TMInput, TMOutput, TMTransitionFunction}

/**
  * Created by k_higuchi on 2017/07/15.
  */
case class DTMTransitionFunctions(values: Set[TMTransitionFunction]) {

  def transitionTo(input: TMInput): Option[TMOutput] =
    values
      .find(_.canApplyTo(input))
      .map(_.output)

}

object DTMTransitionFunctions {
  def apply(values: TMTransitionFunction*): DTMTransitionFunctions = new DTMTransitionFunctions(Set(values:_*))
}
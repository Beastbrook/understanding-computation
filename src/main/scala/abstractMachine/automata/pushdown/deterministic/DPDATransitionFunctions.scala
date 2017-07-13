package abstractMachine.automata.pushdown.deterministic

import abstractMachine.automata.pushdown.{PDAInput, PDAOutput, PDATransitionFunction}

/**
  * Created by k_higuchi on 2017/07/09.
  */
case class DPDATransitionFunctions(values: Set[PDATransitionFunction]) {

  def transitionTo(input: PDAInput): Option[PDAOutput] =
    values
      .find(_.canApplyTo(input))
      .map(_.output)

}

object DPDATransitionFunctions {
  def apply(values: PDATransitionFunction*): DPDATransitionFunctions =
    DPDATransitionFunctions(Set(values:_*))
}

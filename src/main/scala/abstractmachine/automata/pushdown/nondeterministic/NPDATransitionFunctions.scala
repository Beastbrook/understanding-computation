package abstractmachine.automata.pushdown.nondeterministic

import abstractmachine.automata.pushdown.{PDAInput, PDAOutput, PDATransitionFunction}

/**
  * Created by k_higuchi on 2017/07/10.
  */
case class NPDATransitionFunctions(values: Set[PDATransitionFunction]) {

  def transitionTo(inputs: Set[PDAInput]): Set[PDAOutput] =
    for {
      func <- values
      input <- inputs
      if func.input == input
    } yield func.output

  def transitionTo(input: PDAInput): Set[PDAOutput] =
    values
      .filter(_.input == input)
      .map(_.output)

}

object NPDATransitionFunctions {
  def apply(values: PDATransitionFunction*): NPDATransitionFunctions =
    NPDATransitionFunctions(Set(values:_*))
}

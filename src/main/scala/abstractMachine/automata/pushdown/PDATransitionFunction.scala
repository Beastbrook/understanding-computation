package abstractMachine.automata.pushdown

import abstractMachine.automata.{State, Character}

/**
  * Created by k_higuchi on 2017/07/09.
  */
case class PDATransitionFunction(input: PDAInput, output: PDAOutput) {
  def canApplyTo(input: PDAInput): Boolean = input == this.input
}

case class PDAInput(
  state: State,
  character: Character,
  popCharacter: Character
)

case class PDAOutput(
  nextState: State,
  pushCharacters: Seq[Character]
)

object PDATransitionFunction {

  def apply(
    state: State,
    character: Character,
    popCharacter: Character
  )(
    nextState: State,
    pushCharacters: Seq[Character]
  ): PDATransitionFunction = {
    val input = PDAInput(state, character, popCharacter)
    val output = PDAOutput(nextState, pushCharacters)
    PDATransitionFunction(input, output)
  }

}
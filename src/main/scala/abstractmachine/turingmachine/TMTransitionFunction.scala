package abstractmachine.turingmachine

import abstractmachine.{Character, State}

/**
  * Created by k_higuchi on 2017/07/15.
  */
case class TMTransitionFunction(input: TMInput, output: TMOutput) {

  def canApplyTo(input: TMInput): Boolean = input == this.input

}

case class TMInput(state: State, readChar: Character)
case class TMOutput(state: State, writeChar: Character, direction: HeadMoveDirection)

object TMTransitionFunction {

  def apply(state: State, readChar: Character)
           (nextState: State, writeChar: Character, direction: HeadMoveDirection): TMTransitionFunction =
    new TMTransitionFunction(TMInput(state, readChar), TMOutput(nextState, writeChar, direction))

}
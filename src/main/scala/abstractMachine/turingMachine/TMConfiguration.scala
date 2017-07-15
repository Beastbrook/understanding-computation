package abstractMachine.turingMachine

import abstractMachine.State

/**
  * Created by k_higuchi on 2017/07/15.
  */
case class TMConfiguration(state: State, tape: Tape) {

  def nextConfiguration(output: TMOutput): TMConfiguration =
    TMConfiguration(output.state, tape.write(output.writeChar).move(output.direction))

}

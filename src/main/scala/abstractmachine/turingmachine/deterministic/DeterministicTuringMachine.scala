package abstractmachine.turingmachine.deterministic

import abstractmachine.turingmachine.{TMConfiguration, TMInput, Tape}
import abstractmachine.{NonEmptyChar, State}

import scala.annotation.tailrec

/**
  * Created by k_higuchi on 2017/07/15.
  */
case class DeterministicTuringMachine(
  configuration: TMConfiguration,
  acceptStates: Set[State],
  transitionFunctions: DTMTransitionFunctions,
  isStuck: Boolean
) {

  val isAcceptState: Boolean = acceptStates.contains(configuration.state)

  def step: DeterministicTuringMachine = {
    this.transitionFunctions.transitionTo(
      TMInput(configuration.state, configuration.tape.middle)
    ).fold(this.copy(isStuck = true)) { output =>
      val nextConf = configuration.nextConfiguration(output)
      this.copy(configuration = nextConf)
    }
  }

  def run: DeterministicTuringMachine = {
    @tailrec
    def loop(dtm: DeterministicTuringMachine): DeterministicTuringMachine = {
      if(dtm.isAcceptState || dtm.isStuck) dtm
      else loop(dtm.step)
    }
    loop(this)
  }

}

object DeterministicTuringMachine {

  def apply(
    state: State = State(1),
    tapeString: String,
    acceptStates: Set[State],
    transitionFunctions: DTMTransitionFunctions,
    isStuck: Boolean = false
  ): DeterministicTuringMachine = {

    val tape = Tape(middle = NonEmptyChar(tapeString.head), right = tapeString.tail.map(NonEmptyChar))
    val conf = TMConfiguration(state, tape)

    DeterministicTuringMachine(conf, acceptStates, transitionFunctions, isStuck)
  }

}

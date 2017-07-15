package abstractMachine.automata.pushdown.nondeterministic

import abstractMachine.automata.pushdown.{PDAInput, PDAStack}
import abstractMachine.{Character, EmptyChar, NonEmptyChar, State}

import scala.annotation.tailrec

/**
  * Created by k_higuchi on 2017/07/10.
  */
class NondeterministicPushDownAutomata private(
  val configurations: Set[NPDAConfiguration],
  val acceptStates: Set[State],
  val transitionFunctions: NPDATransitionFunctions,
  val freeMoveTransFuncs: NPDATransitionFunctions,
  val nonFreeMoveTransFuncs: NPDATransitionFunctions,
  val isStuck: Boolean
) {

  val isAcceptState: Boolean = {
    val currentStates = this.configurations.map(_.currentState)
    acceptStates.intersect(currentStates).nonEmpty && !this.isStuck
  }

  private def read(character: Character): NondeterministicPushDownAutomata = {

    val nextConfs = NondeterministicPushDownAutomata.nextConfigurations(
      character = character,
      transitionFunctions = this.nonFreeMoveTransFuncs,
      configurations = this.configurations
    )

    if(nextConfs.isEmpty) this.copy(isStuck = true)
    else this.copy(configurations = nextConfs)
  }

  def read(string: String): NondeterministicPushDownAutomata =
    string.foldLeft(this)((pda, char) => pda.read(NonEmptyChar(char)))

  def copy(
    configurations: Set[NPDAConfiguration] = this.configurations,
    acceptStates: Set[State] = this.acceptStates,
    transitionFunctions: NPDATransitionFunctions = this.transitionFunctions,
    isStuck: Boolean = this.isStuck
  ): NondeterministicPushDownAutomata =
    NondeterministicPushDownAutomata.apply(
      configurations = configurations,
      acceptStates = acceptStates,
      transitionFunctions = transitionFunctions,
      isStuck = isStuck
    )

}

case class NPDAConfiguration(currentState: State, stack: PDAStack)

object NondeterministicPushDownAutomata {

  def apply(
    configurations: Set[NPDAConfiguration] = Set(NPDAConfiguration(State(1), PDAStack(NonEmptyChar('$')))),
    acceptStates: Set[State],
    transitionFunctions: NPDATransitionFunctions,
    isStuck: Boolean = false
  ): NondeterministicPushDownAutomata = {

    val (freeMoveTransFuncs, nonFreeMoveTransFuncs) =
      transitionFunctions.values
        .partition(_.input.character == EmptyChar) match {
        case (freeMove, nonFreeMove) =>
          (NPDATransitionFunctions(freeMove), NPDATransitionFunctions(nonFreeMove))
      }

    val freeMovedConfigurations = this.freeMove(freeMoveTransFuncs, configurations)

    new NondeterministicPushDownAutomata(
      configurations = freeMovedConfigurations,
      acceptStates = acceptStates,
      transitionFunctions = transitionFunctions,
      freeMoveTransFuncs = freeMoveTransFuncs,
      nonFreeMoveTransFuncs = nonFreeMoveTransFuncs,
      isStuck = isStuck
    )
  }

  private def freeMove(
    freeMoveTransFuncs: NPDATransitionFunctions,
    configurations: Set[NPDAConfiguration]
  ): Set[NPDAConfiguration] = {

    @tailrec
    def loop(confs: Set[NPDAConfiguration], acc: Set[NPDAConfiguration]): Set[NPDAConfiguration] = {
      val nextConfs = nextConfigurations(EmptyChar, freeMoveTransFuncs, confs)
      if(nextConfs.isEmpty) acc
      else loop(nextConfs, acc.union(nextConfs))
    }

    loop(configurations, configurations)
  }

  private def nextConfigurations(
                                  character: Character,
                                  transitionFunctions: NPDATransitionFunctions,
                                  configuration: NPDAConfiguration
  ): Set[NPDAConfiguration] = {

    if(configuration.stack.isEmpty) return Set()

    val stackPopResult = configuration.stack.pop

    val input = PDAInput(
      state = configuration.currentState,
      character = character,
      popCharacter = stackPopResult.popCharacter
    )

    val outputs = transitionFunctions.transitionTo(input)

    if(outputs.isEmpty) Set()
    else
      outputs.map { output =>
        NPDAConfiguration(output.nextState, stackPopResult.stack.push(output.pushCharacters))
      }
  }

  private def nextConfigurations(
                                  character: Character,
                                  transitionFunctions: NPDATransitionFunctions,
                                  configurations: Set[NPDAConfiguration]
  ): Set[NPDAConfiguration] =
    configurations
      .flatMap(this.nextConfigurations(character, transitionFunctions, _))

}
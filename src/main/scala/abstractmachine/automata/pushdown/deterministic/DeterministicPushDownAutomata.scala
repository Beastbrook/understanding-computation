package abstractmachine.automata.pushdown.deterministic

import abstractmachine.{Character, EmptyChar, NonEmptyChar, State}
import abstractmachine.automata.pushdown.{PDAInput, PDAStack}

import scala.annotation.tailrec

/**
  * Created by k_higuchi on 2017/07/09.
  */
class DeterministicPushDownAutomata private(
  val currentState: State,
  val stack: PDAStack,
  val acceptStates: Set[State],
  val transitionFunctions: DPDATransitionFunctions,
  val freeMoveTransFuncs: DPDATransitionFunctions,
  val nonFreeMoveTransFuncs: DPDATransitionFunctions,
  val isStuck: Boolean
) {

  val isAcceptState: Boolean =
    acceptStates.contains(this.currentState) && !this.isStuck

  private def read(character: Character): DeterministicPushDownAutomata = {

    if(this.stack.isEmpty) return this.copy(isStuck = true)

    val popResult = this.stack.pop

    val input = PDAInput(
      state = this.currentState,
      character = character,
      popCharacter = popResult.popCharacter
    )

    val output = this.transitionFunctions.transitionTo(input)

    output match {
      case None => this.copy(isStuck = true)
      case Some(out) => this.copy(
        currentState = out.nextState,
        stack = popResult.stack.push(out.pushCharacters)
      )
    }
  }

  def read(string: String): DeterministicPushDownAutomata =
    string.foldLeft(this)((pda, char) => pda.read(NonEmptyChar(char)))

  def copy(
    currentState: State = this.currentState,
    stack: PDAStack = this.stack,
    acceptStates: Set[State] = this.acceptStates,
    transitionFunctions: DPDATransitionFunctions = this.transitionFunctions,
    isStuck: Boolean = this.isStuck
  ): DeterministicPushDownAutomata =
    DeterministicPushDownAutomata.apply(
      currentState = currentState,
      stack = stack,
      acceptStates = acceptStates,
      transitionFunctions = transitionFunctions,
      isStuck = isStuck
    )

}

object DeterministicPushDownAutomata {

  def apply(
    currentState: State = State(1),
    stack: PDAStack = PDAStack(NonEmptyChar('$')),
    acceptStates: Set[State],
    transitionFunctions: DPDATransitionFunctions,
    isStuck: Boolean = false
  ): DeterministicPushDownAutomata = {

    val (freeMoveTransFuncs, nonFreeMoveTransFuncs) =
      transitionFunctions.values
        .partition(_.input.character == EmptyChar) match {
        case (freeMove, nonFreeMove) => (DPDATransitionFunctions(freeMove), DPDATransitionFunctions(nonFreeMove))
      }

    val freeMoveResult = this.freeMove(freeMoveTransFuncs, currentState, stack)

    new DeterministicPushDownAutomata(
      currentState = freeMoveResult.nextState,
      stack = freeMoveResult.nextStack,
      acceptStates = acceptStates,
      transitionFunctions = transitionFunctions,
      freeMoveTransFuncs = freeMoveTransFuncs,
      nonFreeMoveTransFuncs = nonFreeMoveTransFuncs,
      isStuck = isStuck
    )
  }

  private def freeMove(
    freeMoveTransFuncs: DPDATransitionFunctions,
    currentState: State,
    stack: PDAStack
  ): FreeMoveResult = {

    @tailrec
    def loop(crtStt: State, stk: PDAStack): FreeMoveResult = {

      if(stk.isEmpty) return FreeMoveResult(crtStt, stk)

      val stackPopResult = stk.pop
      val next =
        freeMoveTransFuncs
          .transitionTo(PDAInput(crtStt, EmptyChar, stackPopResult.popCharacter))

      next match {
        case None => FreeMoveResult(crtStt, stk)
        case Some(output) => loop(
          output.nextState, stackPopResult.stack.push(output.pushCharacters)
        )
      }
    }
    loop(currentState, stack)
  }

  private case class FreeMoveResult(nextState: State, nextStack: PDAStack)

}
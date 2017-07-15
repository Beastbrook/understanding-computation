package abstractMachine.automata.finite.nondeterministic

import abstractMachine.{Character, EmptyChar, NonEmptyChar, State}

import scala.annotation.tailrec

/**
  * Created by k_higuchi on 2017/07/08.
  */
class NondeterministicFiniteAutomata private(
  val currentStates: Set[State],
  val acceptStates: Set[State],
  val rulebook: NFARulebook,
  val nonFreeMoveRulebook: NFARulebook,
  val freeMoveRulebook: NFARulebook
) {

  val isAcceptState: Boolean = this.currentStates.intersect(acceptStates).nonEmpty

  private def nextStates(character: Character): Set[State] = {
    this.nonFreeMoveRulebook.nextStates(currentStates, character)
  }

  private def read(character: Character): NondeterministicFiniteAutomata = {
    val nextStates = this.nextStates(character)
    this.copy(currentStates = nextStates)
  }

  def read(string: String): NondeterministicFiniteAutomata =
    string.foldLeft(this)( (dfa, char) => dfa.read(NonEmptyChar(char)) )

  def copy(
    currentStates: Set[State] = this.currentStates,
    acceptStates: Set[State] = this.acceptStates,
    rulebook: NFARulebook = this.rulebook
  ): NondeterministicFiniteAutomata =
    NondeterministicFiniteAutomata.apply(
      currentStates = currentStates,
      acceptStates = acceptStates,
      rulebook = rulebook
    )

}

object NondeterministicFiniteAutomata {

  def apply(
    currentStates: Set[State] = Set(State(1)),
    acceptStates: Set[State],
    rulebook: NFARulebook
  ): NondeterministicFiniteAutomata = {

    val (freeMoveRulebook, nonFreeMoveRulebook) =
      rulebook.rules.partition(_.character == EmptyChar) match {
        case (freeMove, nonFreeMove) => (NFARulebook(freeMove), NFARulebook(nonFreeMove))
      }

    new NondeterministicFiniteAutomata(
      currentStates = this.freeMove(freeMoveRulebook, currentStates),
      acceptStates = acceptStates,
      rulebook = rulebook,
      nonFreeMoveRulebook = nonFreeMoveRulebook,
      freeMoveRulebook = freeMoveRulebook
    )

  }

  private def freeMove(freeMoveRulebook: NFARulebook, currentStates: Set[State]): Set[State] = {
    @tailrec
    def loop(crtSts: Set[State]): Set[State] = {
      val nextStates = freeMoveRulebook.nextStates(crtSts, EmptyChar)
      if(nextStates.isEmpty) crtSts
      else loop(nextStates)
    }
    currentStates.union(loop(currentStates))
  }

}
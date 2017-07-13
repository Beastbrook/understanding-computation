package abstractMachine.automata.finite.deterministic

import abstractMachine.automata.{NonEmptyChar, State}
import abstractMachine.automata.finite.FiniteAutomataRule

/**
  * Created by k_higuchi on 2017/07/08.
  */
object DFASimulation extends App {

  val rulebook = DFARulebook(Set(
    FiniteAutomataRule(State(1), NonEmptyChar('a'), State(2)),
    FiniteAutomataRule(State(1), NonEmptyChar('b'), State(1)),
    FiniteAutomataRule(State(2), NonEmptyChar('a'), State(2)),
    FiniteAutomataRule(State(2), NonEmptyChar('b'), State(3)),
    FiniteAutomataRule(State(3), NonEmptyChar('a'), State(3)),
    FiniteAutomataRule(State(3), NonEmptyChar('b'), State(3))
  ))

  val dfa1 = DeterministicFiniteAutomata(
    acceptStates = Set(State(1), State(3)),
    rulebook = rulebook
  )

  val dfa1Result1 = dfa1.read("abb")
  val dfa1Result2 = dfa1.read("aaaaa")
  val dfa1Result3 = dfa1.read("bbbb")
  val dfa1Result4 = dfa1.read("abaaaaabbbb")

  def createMessage(string: String, dfa: DeterministicFiniteAutomata) =
    s"input: '$string', accept result: ${dfa.isAcceptState}, Current: ${dfa.currentState}"

  println(createMessage("abb", dfa1Result1))
  println(createMessage("aaaaa", dfa1Result2))
  println(createMessage("bbbb", dfa1Result3))
  println(createMessage("abaaaaabbbb", dfa1Result4))

}

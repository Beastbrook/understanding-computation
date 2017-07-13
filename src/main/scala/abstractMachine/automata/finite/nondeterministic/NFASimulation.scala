package abstractMachine.automata.finite.nondeterministic

import abstractMachine.automata.{EmptyChar, NonEmptyChar, State}
import abstractMachine.automata.finite.FiniteAutomataRule

/**
  * Created by k_higuchi on 2017/07/08.
  */
object NFASimulation extends App {
  
  val rulebook1 = NFARulebook(Set(
    FiniteAutomataRule(State(1), NonEmptyChar('a'), State(1)),
    FiniteAutomataRule(State(1), NonEmptyChar('b'), State(1)),
    FiniteAutomataRule(State(1), NonEmptyChar('b'), State(2)),
    FiniteAutomataRule(State(2), NonEmptyChar('a'), State(3)),
    FiniteAutomataRule(State(2), NonEmptyChar('b'), State(3)),
    FiniteAutomataRule(State(3), NonEmptyChar('a'), State(4)),
    FiniteAutomataRule(State(3), NonEmptyChar('b'), State(4))
  ))

  val nfa1 = NondeterministicFiniteAutomata(
    acceptStates = Set(State(4)),
    rulebook = rulebook1
  )

  val nfa1Result1 = nfa1.read("abb")
  val nfa1Result2 = nfa1.read("aaaaa")
  val nfa1Result3 = nfa1.read("bbbb")
  val nfa1Result4 = nfa1.read("abaaaaabbbb")

  def createMessage(string: String, nfa: NondeterministicFiniteAutomata) =
    s"input: '$string', accept result: ${nfa.isAcceptState}, Current: ${nfa.currentStates}"

  println("NFA1")
  println(createMessage("abb", nfa1Result1))
  println(createMessage("aaaaa", nfa1Result2))
  println(createMessage("bbbb", nfa1Result3))
  println(createMessage("abaaaaabbbb", nfa1Result4))


  // aの数が2の倍数 or 3の倍数で受理
  // 1 -> 2 and 1 -> 4
  // 2の倍数: 2 <-> 3
  // 3の倍数: 4 -> 5 -> 6 -> 4 -> 5 -> 6 ->...
  val rulebook2 = NFARulebook(Set(
    FiniteAutomataRule(State(1), EmptyChar, State(2)),
    FiniteAutomataRule(State(1), EmptyChar, State(4)),
    FiniteAutomataRule(State(2), NonEmptyChar('a'), State(3)),
    FiniteAutomataRule(State(3), NonEmptyChar('a'), State(2)),
    FiniteAutomataRule(State(4), NonEmptyChar('a'), State(5)),
    FiniteAutomataRule(State(5), NonEmptyChar('a'), State(6)),
    FiniteAutomataRule(State(6), NonEmptyChar('a'), State(4))
  ))

  val nfa2 = NondeterministicFiniteAutomata(
    acceptStates = Set(State(2),State(4)),
    rulebook = rulebook2
  )

  val empty = ""
  val a1 = "a"
  val a2 = "aa"
  val a3 = "aaa"
  val a5 = "aaaaa"

  val nfa2Result0 = nfa2.read(empty)
  val nfa2Result1 = nfa2.read(a1)
  val nfa2Result2 = nfa2.read(a2)
  val nfa2Result3 = nfa2.read(a3)
  val nfa2Result4 = nfa2.read(a5)

  println("NFA2")
  println(createMessage(empty, nfa2Result0))
  println(createMessage(a1, nfa2Result1))
  println(createMessage(a2, nfa2Result2))
  println(createMessage(a3, nfa2Result3))
  println(createMessage(a5, nfa2Result4))

  // スター演算
  val rulebook3 = NFARulebook(Set(
    FiniteAutomataRule(State(1), EmptyChar, State(2)),
    FiniteAutomataRule(State(2), NonEmptyChar('a'), State(3)),
    FiniteAutomataRule(State(3), EmptyChar, State(2))
  ))

  val nfa3 = NondeterministicFiniteAutomata(
    acceptStates = Set(State(1), State(3)),
    rulebook = rulebook3
  )

  val nfa3Result1 = nfa3.read(empty)
  val nfa3Result2 = nfa3.read(a1)
  val nfa3Result3 = nfa3.read(a2)

  println("nfa3")
  println(createMessage(empty, nfa3Result1))
  println(createMessage(a1, nfa3Result2))
  println(createMessage(a2, nfa3Result3))

}

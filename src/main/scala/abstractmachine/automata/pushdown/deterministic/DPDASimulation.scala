package abstractmachine.automata.pushdown.deterministic

import abstractmachine.automata.pushdown.{PDATransitionFunction => TransFunc}
import abstractmachine.{EmptyChar, State, NonEmptyChar => NEChar}

/**
  * Created by k_higuchi on 2017/07/09.
  */
object DPDASimulation extends App {

  def createMessage(string: String, dpda: DeterministicPushDownAutomata): String =
    s"input: '$string', accept: ${dpda.isAcceptState}," +
      s" Current: ${dpda.currentState}, isStuck: ${dpda.isStuck}, ${dpda.stack}"

  val transitionFunctions1 = DPDATransitionFunctions(
    TransFunc(State(1), NEChar('('), NEChar('$'))(State(2), Seq(NEChar('b'), NEChar('$'))),
    TransFunc(State(2), NEChar('('), NEChar('b'))(State(2), Seq(NEChar('b'), NEChar('b'))),
    TransFunc(State(2), NEChar(')'), NEChar('b'))(State(2), Seq()),
    TransFunc(State(2), EmptyChar, NEChar('$'))(State(1), Seq(NEChar('$')))
  )

  val dpda1 = DeterministicPushDownAutomata(
    acceptStates = Set(State(1)),
    transitionFunctions = transitionFunctions1
  )

  val empty = ""
  val string1 = ")"
  val string2 = "()"
  val string3 = "(()"
  val string4 = "((()()))("
  val string5 = "((()()))()"

  val dpda1Result0 = dpda1.read(empty)
  val dpda1Result1 = dpda1.read(string1)
  val dpda1Result2 = dpda1.read(string2)
  val dpda1Result3 = dpda1.read(string3)
  val dpda1Result4 = dpda1.read(string4)
  val dpda1Result5 = dpda1.read(string5)

  println(createMessage(empty, dpda1Result0))
  println(createMessage(string1, dpda1Result1))
  println(createMessage(string2, dpda1Result2))
  println(createMessage(string3, dpda1Result3))
  println(createMessage(string4, dpda1Result4))
  println(createMessage(string5, dpda1Result5))

}

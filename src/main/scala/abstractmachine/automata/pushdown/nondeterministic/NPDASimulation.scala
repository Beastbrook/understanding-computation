package abstractmachine.automata.pushdown.nondeterministic

import abstractmachine.{EmptyChar, State, NonEmptyChar => NEChar}
import abstractmachine.automata.pushdown.{PDATransitionFunction => TransFunc}

/**
  * Created by k_higuchi on 2017/07/11.
  */
object NPDASimulation extends App {

  def createMessage(string: String, npda: NondeterministicPushDownAutomata): String = {
    val npdaResult = npda.read(string)

    s"input: '$string', accept: ${npdaResult.isAcceptState}, " +
    s"Current: ${npdaResult.configurations.mkString(", ")}, " +
    s"isStuck: ${npdaResult.isStuck}"
  }


  val transitionFunctions1 = NPDATransitionFunctions(
    TransFunc(State(1), NEChar('a'), NEChar('$'))(State(1), Seq(NEChar('a'), NEChar('$'))),
    TransFunc(State(1), NEChar('a'), NEChar('a'))(State(1), Seq(NEChar('a'), NEChar('a'))),
    TransFunc(State(1), NEChar('a'), NEChar('b'))(State(1), Seq(NEChar('a'), NEChar('b'))),
    TransFunc(State(1), NEChar('b'), NEChar('$'))(State(1), Seq(NEChar('b'), NEChar('$'))),
    TransFunc(State(1), NEChar('b'), NEChar('a'))(State(1), Seq(NEChar('b'), NEChar('a'))),
    TransFunc(State(1), NEChar('b'), NEChar('b'))(State(1), Seq(NEChar('b'), NEChar('b'))),
    TransFunc(State(1), EmptyChar, NEChar('$'))(State(2), Seq(NEChar('$'))),
    TransFunc(State(1), EmptyChar, NEChar('a'))(State(2), Seq(NEChar('a'))),
    TransFunc(State(1), EmptyChar, NEChar('b'))(State(2), Seq(NEChar('b'))),
    TransFunc(State(2), NEChar('a'), NEChar('a'))(State(2), Seq()),
    TransFunc(State(2), NEChar('b'), NEChar('b'))(State(2), Seq()),
    TransFunc(State(2), EmptyChar, NEChar('$'))(State(3), Seq(NEChar('$')))
  )

  val npda1 = NondeterministicPushDownAutomata(
    acceptStates = Set(State(3)),
    transitionFunctions = transitionFunctions1
  )

  val empty = ""
  val string1 = "a"
  val string2 = "abb"
  val string3 = "abba"
  val string4 = "baabaa"
  val string5 = "babbaabbab"
  val string6 = "abaabaabaaba"

  println(createMessage(empty, npda1))
  println(createMessage(string1, npda1))
  println(createMessage(string2, npda1))
  println(createMessage(string3, npda1))
  println(createMessage(string4, npda1))
  println(createMessage(string5, npda1))
  println(createMessage(string6, npda1))

  val startTransFuncs = NPDATransitionFunctions(
    // start state rule
    TransFunc(State(1), EmptyChar, NEChar('$'))(State(2), Seq(NEChar('S'), NEChar('$')))
  )

  // CFG -> NPDA
  val symbolTransFuncs = NPDATransitionFunctions(
    // <statement> ::= <while> | <assign>
    TransFunc(State(2), EmptyChar, NEChar('S'))(State(2), Seq(NEChar('W'))),
    TransFunc(State(2), EmptyChar, NEChar('S'))(State(2), Seq(NEChar('A'))),

    // <while> ::= 'w' '(' <expression> ')' '{' <statement> '}'
    TransFunc(State(2), EmptyChar, NEChar('W'))(State(2), Seq(NEChar('w'), NEChar('('), NEChar('E'), NEChar(')'), NEChar('{'), NEChar('S'), NEChar('}'))),

    // <assign> ::= 'v' '=' <expression>
    TransFunc(State(2), EmptyChar, NEChar('A'))(State(2), Seq(NEChar('v'), NEChar('='), NEChar('E'))),

    // <expression> ::= <less-than>
    TransFunc(State(2), EmptyChar, NEChar('E'))(State(2), Seq(NEChar('L'))),

    // <less-than> ::= <multiply> '<' <less-than> | <multiply>
    TransFunc(State(2), EmptyChar, NEChar('L'))(State(2), Seq(NEChar('M'), NEChar('<'), NEChar('L'))),
    TransFunc(State(2), EmptyChar, NEChar('L'))(State(2), Seq(NEChar('M'))),

    // <multiply> ::= <term> '*' <multiply> | <term>
    TransFunc(State(2), EmptyChar, NEChar('M'))(State(2), Seq(NEChar('T'), NEChar('*'), NEChar('M'))),
    TransFunc(State(2), EmptyChar, NEChar('M'))(State(2), Seq(NEChar('T'))),

    // <term> ::= 'n' | 'v'
    TransFunc(State(2), EmptyChar, NEChar('T'))(State(2), Seq(NEChar('n'))),
    TransFunc(State(2), EmptyChar, NEChar('T'))(State(2), Seq(NEChar('v')))
  )

  val tokenTransFuncs = NPDATransitionFunctions(
    TransFunc(State(2), NEChar('i'), NEChar('i'))(State(2), Seq()),
    TransFunc(State(2), NEChar('e'), NEChar('e'))(State(2), Seq()),
    TransFunc(State(2), NEChar('w'), NEChar('w'))(State(2), Seq()),
    TransFunc(State(2), NEChar('d'), NEChar('d'))(State(2), Seq()),
    TransFunc(State(2), NEChar('('), NEChar('('))(State(2), Seq()),
    TransFunc(State(2), NEChar(')'), NEChar(')'))(State(2), Seq()),
    TransFunc(State(2), NEChar('{'), NEChar('{'))(State(2), Seq()),
    TransFunc(State(2), NEChar('}'), NEChar('}'))(State(2), Seq()),
    TransFunc(State(2), NEChar(';'), NEChar(';'))(State(2), Seq()),
    TransFunc(State(2), NEChar('='), NEChar('='))(State(2), Seq()),
    TransFunc(State(2), NEChar('+'), NEChar('+'))(State(2), Seq()),
    TransFunc(State(2), NEChar('*'), NEChar('*'))(State(2), Seq()),
    TransFunc(State(2), NEChar('<'), NEChar('<'))(State(2), Seq()),
    TransFunc(State(2), NEChar('n'), NEChar('n'))(State(2), Seq()),
    TransFunc(State(2), NEChar('b'), NEChar('b'))(State(2), Seq()),
    TransFunc(State(2), NEChar('v'), NEChar('v'))(State(2), Seq())
  )

  val acceptTransFuncs = NPDATransitionFunctions(
    // accept state rule
    TransFunc(State(2), EmptyChar, NEChar('$'))(State(3), Seq(NEChar('$')))
  )

  val transitionFunctionsForCFG = NPDATransitionFunctions(
    startTransFuncs.values ++ symbolTransFuncs.values ++ tokenTransFuncs.values ++ acceptTransFuncs.values
  )

  val npda2 = NondeterministicPushDownAutomata(
    acceptStates = Set(State(3)),
    transitionFunctions = transitionFunctionsForCFG
  )

  val whileTokenString = "w(v<n){v=v*n}"

  println(createMessage(whileTokenString, npda2))

}

package abstractMachine.turingMachine.deterministic

import abstractMachine.turingMachine.{Left, Right, TMConfiguration, Tape, TMTransitionFunction => TransFunc}
import abstractMachine.{State, NonEmptyChar => NEChar}

/**
  * Created by k_higuchi on 2017/07/15.
  */
object DTMSimulate extends App {

  val tape = Tape(middle = NEChar('x'), blank = NEChar('_'))

  println(tape)
  println(tape.move(Left))
  println(tape.move(Left).move(Left))

  println(tape)
  println(tape.move(Right))
  println(tape.move(Right).move(Right))

  // 二進数のインクリメント
  val transitionFunctions1 = DTMTransitionFunctions(
    TransFunc(State(1), NEChar('0'))(State(2), NEChar('1'), Right),
    TransFunc(State(1), NEChar('1'))(State(1), NEChar('0'), Left),
    TransFunc(State(1), NEChar('_'))(State(2), NEChar('1'), Right),
    TransFunc(State(2), NEChar('0'))(State(2), NEChar('0'), Right),
    TransFunc(State(2), NEChar('1'))(State(2), NEChar('1'), Right),
    TransFunc(State(2), NEChar('_'))(State(3), NEChar('_'), Left)
  )

  // 右端の数字からじゃないとうまく動かない
  val tape1 = Tape(left = Seq(NEChar('1'), NEChar('0'), NEChar('1')), middle = NEChar('1'))

  val dtm1 = DeterministicTuringMachine(
    configuration = TMConfiguration(State(1), tape1),
    acceptStates = Set(State(3)),
    transitionFunctions = transitionFunctions1,
    isStuck = false
  )

  println(dtm1.configuration.tape)
  println(dtm1.run.configuration.tape)


  // aaabbbcccのような回文を認識する(PDAにできないこと)
  val transitionFunctions2 = DTMTransitionFunctions(
    // State1: aを探して右にスキャンする
    TransFunc(State(1), NEChar('X'))(State(1), NEChar('X'), Right),
    TransFunc(State(1), NEChar('a'))(State(2), NEChar('X'), Right),
    TransFunc(State(1), NEChar('_'))(State(6), NEChar('_'), Left), // 受理状態

    // State2: bを探して右にスキャンする
    TransFunc(State(2), NEChar('X'))(State(2), NEChar('X'), Right),
    TransFunc(State(2), NEChar('a'))(State(2), NEChar('a'), Right),
    TransFunc(State(2), NEChar('b'))(State(3), NEChar('X'), Right),

    // State3: cを探して右にスキャンする
    TransFunc(State(3), NEChar('X'))(State(3), NEChar('X'), Right),
    TransFunc(State(3), NEChar('b'))(State(3), NEChar('b'), Right),
    TransFunc(State(3), NEChar('c'))(State(4), NEChar('X'), Right),

    // State4: 文字の末尾を探して右にスキャンする
    TransFunc(State(4), NEChar('c'))(State(4), NEChar('c'), Right),
    TransFunc(State(4), NEChar('_'))(State(5), NEChar('_'), Left),

    // State5: 文字の先頭を探して左にスキャンする
    TransFunc(State(5), NEChar('a'))(State(5), NEChar('a'), Left),
    TransFunc(State(5), NEChar('b'))(State(5), NEChar('b'), Left),
    TransFunc(State(5), NEChar('c'))(State(5), NEChar('c'), Left),
    TransFunc(State(5), NEChar('X'))(State(5), NEChar('X'), Left),
    TransFunc(State(5), NEChar('_'))(State(1), NEChar('_'), Right)
  )

  val dtm2 = DeterministicTuringMachine(
    tapeString = "aaabbbccc",
    acceptStates = Set(State(6)),
    transitionFunctions = transitionFunctions2
  )

  val dtm2Run = dtm2.run

  println(dtm2.configuration.tape)
  println(dtm2Run.configuration.tape)
  println(dtm2Run.run.isAcceptState)

}

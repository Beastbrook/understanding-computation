package abstractmachine.turingmachine

/**
  * Created by k_higuchi on 2017/07/15.
  */
sealed trait HeadMoveDirection
case object Left extends HeadMoveDirection
case object Right extends HeadMoveDirection

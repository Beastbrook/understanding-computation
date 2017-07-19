package lambdacalculus.untyped

/**
  * Created by k_higuchi on 2017/07/16.
  */
object ChurchBooleans {

  type ChurchBoolean = Any => Any => Any

  val True: ChurchBoolean = x => y => x
  val False: ChurchBoolean = x => y => y
  val If: ChurchBoolean => ChurchBoolean = b => b

  def toBoolean(func: ChurchBoolean): Boolean = func(true)(false).asInstanceOf[Boolean]

}

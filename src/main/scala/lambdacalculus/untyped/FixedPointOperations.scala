package lambdacalculus.untyped

/**
  * Created by k_higuchi on 2017/07/16.
  */
object FixedPointOperations {

  type YCombinator = ((Any => Any) => (Any => Any)) => (Any => Any)
  val Y: YCombinator = f => f(Y(f))(_)

//  Y = λf.(λx.f (x x)) (λx.f (x x))
//  val Y: (Any => Any) => (Any => Any) = (f: (Any => Any)) => (x: Any => Any) =>
//    f(x(x.asInstanceOf[Any]).asInstanceOf[Any => Any]).asInstanceOf[Any => Any](
//      (x: (Any => Any)) => f(x(x.asInstanceOf[Any]))
//    ).asInstanceOf[Any => Any]

  // Z = λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))
  type ZCombinator = Any => Any => Any
  val Z: ZCombinator = f =>
    x => f.asInstanceOf[Any=>Any=>Any](
      (y: Any) => x.asInstanceOf[Any=>Any=>Any](x)(y)
    )(
      (x: Any) => f.asInstanceOf[Any=>Any=>Any](
        (y: Any) => x.asInstanceOf[Any=>Any=>Any](x)(y))
    )

}

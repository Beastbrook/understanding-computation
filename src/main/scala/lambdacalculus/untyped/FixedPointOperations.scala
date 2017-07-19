package lambdacalculus.untyped

/**
  * Created by k_higuchi on 2017/07/16.
  */
object FixedPointOperations {

  type YCombinator[A, B] = ((A => B) => (A => B)) => (A => B)

  // Y  = λf.(λx.f(x x))(λx.f(x x))
  // Yg = λf.(λx.f(x x))(λx.f(x x))g  (Yの定義より)
  // Yg = λx.g(x x)(λx.g(x x))        (λfのβ簡約、主関数をgに適用)
  // Yg = λy.g(y y)(λx.g(x x))        (α変換、束縛変数の名前を変える)
  // Yg = g(λx.g(x x)(λx.g(x x)))     (λyのβ簡約、左の関数を右の関数に適用)
  // Yg = g(Yg)                       (第2式より)
  def Y[A, B]: YCombinator[A, B] = f => f(Y(f))

  type ZCombinator[A, B] = ((A => B) => (A => B)) => A => B

  // Z  = λf.(λx.f(λy.x x y))(λx.f(λy.x x y))
  def Z[A, B]: ZCombinator[A, B] = f => x => f(Z(f))(x)

}

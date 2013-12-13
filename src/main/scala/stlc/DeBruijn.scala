package stlc.debruijn

import shapeless._ 
import ops.hlist.At
import syntax.singleton._

import scala.annotation.implicitNotFound

/**
 * Shape types representing the abstract syntax nodes of STLC
 */
object ast {

  sealed case class NumLit(n: Int)
  sealed case class BoolLit(b: Boolean)

  sealed case class Add[L, R](l: L, r: R)
  sealed case class If[C, T, E](cond: C, then: T, els: E)

  /**
   * Explicit type annotation for lambdas
   *
   * @example Lam(Type[Num], Add(Var[_0], NumLit(1)))
   */
  sealed case class Type[T]

  /**
   * AST node for lambda abstractions
   *
   * The argument of the lambda has to be typed explicitly.
   *
   * @example Lam(Type[Num], Add(Var[_0], NumLit(1)))
   */
  sealed case class Lam[T, B](annotation: Type[T], body: B)
  sealed case class App[F, A](fun: F, arg: A)

  sealed case class Var[I <: Nat]()
}

object values {  
  sealed trait Value
  sealed case class Num(n: Int) extends Value
  sealed case class Bool(b: Boolean) extends Value
  sealed case class Fun[-S, +T](f: S => T) extends Value

  type ==>[S, T] = Fun[S, T]
}

@implicitNotFound(msg = "Cannot prove: ${C} ⊢ ${E} is welltyped")
trait Welltyped[C <: HList, E] {
  type Type
  type Context = C
}

object Welltyped {

  import ast._
  import values._
  
  @implicitNotFound(msg = "Cannot prove: ${C} ⊢ ${E} : ${T}")
  type Typed[C <: HList, E, T] = Welltyped[C, E] { type Type = T }

  type As[S, T] = {
    type Term = S
    type Type = T
  }

  // axioms
  implicit def tNum[Γ <: HList]  = new Welltyped[Γ, NumLit] { type Type = Num }
  implicit def tBool[Γ <: HList] = new Welltyped[Γ, BoolLit] { type Type = Bool }

  // The rule for variables performs a lookup in the HList environment Γ
  implicit def tVar[Γ <: HList, N <: Nat](
    implicit s: At[Γ, N]
  ) = new Welltyped[Γ, Var[N]] { type Type = s.Out }
  
  implicit def tAdd[Γ <: HList, L, R](
    implicit
      l: Typed[Γ, L, Num], 
      r: Typed[Γ, R, Num]
  ) = new Welltyped[Γ, Add[L, R]] { type Type = Num }
  
  implicit def tIf[Γ <: HList, C, T, E, S](
    implicit
      c: Typed[Γ, C, Bool],
      t: Typed[Γ, T, S],
      e: Typed[Γ, E, S]
  ) = new Welltyped[Γ, If[C, T, E]] { type Type = S }  

  implicit def tAbs[Γ <: HList, A <: Value, B, R](
    implicit
      b: Typed[A :: Γ, B, R]
  ) = new Welltyped[Γ, Lam[A, B]] { type Type = Fun[A, R] }


  implicit def tApp[Γ <: HList, F, A, S, T](
    implicit
      a: Typed[Γ, A, S],
      f: Typed[Γ, F, Fun[S, T]]
  ) = new Welltyped[Γ, App[F, A]] { type Type = T }

}


object test extends shapeless.Nats {

  import ast._
  import values._
  import Welltyped.Typed

  def welltyped[E](e: E)(implicit ev: Welltyped[HNil, E]) = e

  val e1 = NumLit(3)
  val e2 = Add(NumLit(1), NumLit(5))
  val e3 = Add(NumLit(5), BoolLit(true)) // BAD

  val e4 = Add(If(BoolLit(true), e1, e2), e2)
  val e5 = Add(If(BoolLit(true), e1, BoolLit(false)), e2) // BAD

  val lam = Lam(Type[Num], NumLit(3))
  val lam2 = Lam(Type[Num], Add(Var[_0], Var[_0]))
  val lam3 = Lam(Type[Num], Add(App(lam2, NumLit(5)), Var[_0]))
  val lam4 = Lam(Type[Num],
               Lam(Type[Num], Add(Var[_0], Var[_1])))

  
  val higherorder = App(Lam(Type[Num ==> Num], Var[_0]), lam)
  
  val higherorder2 = App(
    Lam(Type[Num ==> Num], App(Var[_0], NumLit(3))),
    Lam(Type[Num], NumLit(1)))

  val v = Var[_0]
  val app = App(Var[_0], NumLit(3))

  implicitly[Typed[Num :: HNil, Var[_0], Num]]

  implicitly[Typed[Fun[Num, Num] :: HNil, App[Var[_0], NumLit], Num]]

  welltyped(e1)
  welltyped(e2)
  // welltyped(e3) // fails as expected
  welltyped(e4)
  // welltyped(e5) // fails as expected

  welltyped(lam)
  welltyped(lam2)
  welltyped(lam3)
  welltyped(lam4)
  welltyped(higherorder)
  welltyped(higherorder2)
}

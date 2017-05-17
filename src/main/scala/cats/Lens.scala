package cats

import cats.data._

import scala.language.higherKinds

trait Lens[S, A] {
  def modifyF[F[_]: Functor](s: S)(f: A => F[A]): F[S]

  def set(s: S, a: A): S = modify(s)(_ => a)

  def modify(s: S)(f: A => A): S = modifyF[Id](s)(f)

  def get(s: S): A = {
    val storedValue = modifyF[Const[A, ?]](s)(a => Const(a))
    storedValue.getConst
  }
}

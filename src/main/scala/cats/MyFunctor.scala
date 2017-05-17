package cats

// http://gabrielsw.blogspot.sg/2011/08/functors-applicative-functors-and.html
// http://etorreborre.blogspot.sg/2011/06/essence-of-iterator-pattern.html
// https://groups.google.com/forum/#!msg/scala-user/uh5w6N2eAHY/3Shf1295VpYJ

// http://gabrielsw.blogspot.sg/2011/08/functors-applicative-functors-and.html
// http://etorreborre.blogspot.sg/2011/06/essence-of-iterator-pattern.html
// https://groups.google.com/forum/#!msg/scala-user/uh5w6N2eAHY/3Shf1295VpYJ
import scala.language.higherKinds

/// fmap id = id
/// fmap (g . h) = fmap g . fmap h
trait MyFunctor[T[_]] {
  def fmap[A, B](f: A => B)(ta: T[A]): T[B] // (A->B)->(T[A]->T[B])
}

/// Identity     -- pure id <*> v = v
/// Composition  -- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
/// Homomorphism -- pure f <*> pure x = pure (f x)
/// Interchange  -- u <*> pure y = pure ($ y) <*> u
trait MyApplicative[T[_]] extends MyFunctor[T] {
  def pure[A](a: A): T[A] // box it: pure(a)=List(a); pure(b)=Option(a)

  // apply
  def <*>[A, B](tf: T[A => B])(ta: T[A]): T[B]
}

/// right unit    --  m >>= return
/// left unit     --  return >>= f
/// associativity --  (m >>= f) >>= g = m >>= (\x -> f x >>= g)
trait MyMonad[M[_]] extends MyApplicative[M] {
  // flatMap
  def >>=[A, B](ma: M[A])(f: A => M[B]): M[B]
}


import cats._

object TwelfthHomework {

  trait Applicative[F[_]] extends Functor[F] {

    def map[A, B](fa: F[A])(f: A => B): F[B]

    def unit[A](a: => A): F[A]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

    def sequence[A](fsq: List[F[A]]): F[List[A]] = traverse(fsq)(sq => sq)

    def traverse[A, B](trav: List[A])(f: A => F[B]): F[List[B]] = sequence(trav.map(f))
  }

  trait MyMonad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(a => f(a)))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma) { a =>
        map(mb) { b =>
          f(a, b)
        }
      }
  }

  object MonadHomework {
    val optionMonad: Monad[Option] = new Monad[Option] {

      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa.flatMap(f)


      override def pure[A](x: A): Option[A] = Option(x)


      override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
    }

    def eitherMonad[T]: Monad[Either[T, *]] = new Monad[Either[T, *]] {

      override def flatMap[A, B](fa: Either[T, A])(f: A => Either[T, B]): Either[T, B] =
        fa.flatMap(f)


      override def pure[A](x: A): Either[T, A] = Right(x)


      override def tailRecM[A, B](a: A)(f: A => Either[T, Either[A, B]]): Either[T, B] = ???
    }

    def functionMonad[T]: Monad[T => *] = new Monad[T => *] {

      override def flatMap[A, B](fa: T => A)(f: A => T => B): T => B =
        (t: T) => f(fa(t))(t)


      override def pure[A](x: A): T => A = _ => x


      override def tailRecM[A, B](a: A)(f: A => T => Either[A, B]): T => B = ???
    }
  }
}
package part4typeclasses

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future
import cats.Monad

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  // Higher-kinded type class which can tuple elements

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption     = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled       = optionSemigroupal.product(Some(123), None)             // None

  import cats.instances.future._
  // implicit val ec   = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  // val aTupledFuture = Semigroupal[Future].product(Future("The meaning of life"), Future(42))

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  // TODO: implement
  def productWithMonads[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = //(implicit monad: Monad[F])
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  // monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  // MONADS EXTENDS SEMIGROUPALS

  // example: Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // required the implicit Semigroup[List[_]]

  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]

  val eitherCombination = eitherSemigroupal.product(
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This can't be right"))
  )

  // Associativy law: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // TODO 2: define a Semigroupal[List] which does a zip
  def semigroupalList = new Semigroupal[List] {

    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)

  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)
    println(semigroupalList.product(List(1, 2), List("a", "b")))
    // println(productWithMonads(List(1, 2), List("a", "b")))
    // println(invalidsCombination)
    // println(eitherCombination)

  }

}

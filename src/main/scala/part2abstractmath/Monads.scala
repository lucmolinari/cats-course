package part2abstractmath

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList   = List('a', 'b', 'c')

  // TODO 1.1 - how do you create all combinations of (number, char)?
  val combinations = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numberOption = Option(2)
  val charOption   = Option('d')

  // TODO 1.2 - how do you create the combination of (number, char)?
  val combinationOpt = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  /*
    Pattern
    - Wrapping a value into a monadic value
    - the flatMap mechanism

    Monads
   */
  trait MyMonad[M[_]] {
    def pure[A](a: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  // Cats monad
  import cats.Monad
  import cats.instances.option._
  val optionMonad        = Monad[Option]
  val anOption           = optionMonad.pure(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad        = Monad[List]
  val aList            = listMonad.pure(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOptions(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods - weirder imports - pure, flatMap
  import cats.syntax.applicative._ // pure is here
  val oneOption: Option[Int] = 1.pure[Option] // implicit Monad[Option] will be used

  import cats.syntax.flatMap._
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in MyMonad
  // Monads extends Functors
  import cats.syntax.functor._
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  // monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  def main(args: Array[String]): Unit = {
    println(combinations)
    // println(getPairs(numbersList, charsList))
    // println(getPairs(numberOption, charOption))
    println(getPairsFor(numbersList, charsList))
    println(getPairsFor(numberOption, charOption))
  }

}

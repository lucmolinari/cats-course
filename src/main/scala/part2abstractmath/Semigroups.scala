package part2abstractmath

object Semigroups {

  // Semigroups combine elements of the same type
  import cats.Semigroup

  import cats.instances.int._
  val naturalIntSemiGroup = Semigroup[Int]
  val intCombination      = naturalIntSemiGroup.combine(2, 46) // addition

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination      = naturalStringSemigroup.combine("I love ", "Cats")

  def reduceInts(list: List[Int]): Int          = list.reduce(naturalIntSemiGroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO 1: support a new type
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup = Semigroup.instance[Expense] { (a, b) =>
    Expense(Math.max(a.id, b.id), a.amount + b.amount)
  }

  // extension methods from semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3 // requires the presence on an implicit Semigroup[Int]

  // TODO 2: implement reduceThings2
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    // println(intCombination)
    // println(stringCombination)

    val numbers = (1 to 10).toList
    println(reduceInts(numbers))
    println(reduceThings(numbers))

    val strings = List("I'm ", "starting ", "something")
    println(reduceStrings(strings))
    println(reduceThings(strings))

    import cats.instances.option._ // compiler will produce an implicit Semigroup[Option[Int]]
    val numberOptions = numbers.map(n => Option(n))
    println(reduceThings(numberOptions))

    println(reduceThings(List(Option(1), None, Option(3))))
    println(reduceThings(List[Option[Int]](None, None)))

    // Test exercise 1
    val expenses = List(
      Expense(5, 10.5),
      Expense(2, 3),
      Expense(3, 2.5)
    )
    println(reduceThings(expenses)) // Expense(5, 16.0)

    println(Expense(5, 10) |+| Expense(2, 12))

    // Test exercise 2
    println(reduceThings2(expenses)) // Expense(5, 16.0)
    println(reduceThings2(strings))

  }

}

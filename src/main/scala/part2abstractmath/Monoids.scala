package part2abstractmath

import cats.data.Op

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._
  val numbers  = (1 to 1000).toList
  // |+| is always associative
  val sumLeft  = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  // def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  //   list.foldLeft()(_ |+| _)

  // MONOIDS
  import cats.Monoid
  val intMonoid  = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) //1024
  val zero       = intMonoid.empty            //0

  import cats.instances.string._
  val emptyString   = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption   = Monoid[Option[Int]].empty
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)

  // extension methods for Monoids - |+|
  // import cats.syntax.monoid._
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    // Monoid[T].combineAll(list)
    list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  // hint: don't construct a monoid - use an import
  import cats.instances.map._

  val phonebooks = List(
    Map(
      "Alice"   -> 235,
      "Bob"     -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel"  -> 889
    ),
    Map(
      "Tina"    -> 123
    )
  )

  // TODO 3 - shopping cart and online stores with monoids
  // hint: define your monoid - Monoid.instance
  // hint #2: use combineFold
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid =
    Monoid.instance[ShoppingCart](ShoppingCart(Nil, 0), (a, b) => ShoppingCart(a.items ++ b.items, a.total + b.total))

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)

    println(combineFold(List("a", "b")))
    println(combineFold(List(10, 20)))
    println(combineFold(List(Option(10), Option.empty[Int])))

    println(combineFold(phonebooks))

    println(checkout(List(
      ShoppingCart(List("Item 1", "Item 2"), 10),
      ShoppingCart(List("Item 3"), 20)
    )))
  }

}

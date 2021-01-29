package part3datamanipulation

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10)             = countAndSay.run(10).value

  // state = "iterative" computations
  var a                 = 10
  a += 1
  val firstComputation  = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP wiht states
  val firstTransformation   = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondfTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))

  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondfTransformation.map(secondResult => (firstResult, secondResult))
  }

  val compositeTransformation2: State[Int, (String, String)] = for {
    firstResult  <- firstTransformation
    secondResult <- secondfTransformation
  } yield (firstResult, secondResult)

  // function composition is clunky
  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")

  val compositeFunc = func1.andThen { case (newState, firstResult) =>
    (firstResult, func2(newState))
  }

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State { sc =>
    (sc.copy(items = sc.items :+ item, total = sc.total + price), sc.total + price)
  }

  def main(args: Array[String]): Unit = {
    // println(compositeTransformation.run(10).value)
    // println(compositeTransformation2.run(10).value)
    // println(compositeFunc(10))

    val finalShoppingCart = for {
      _ <- addToCart("item1", 20)
      total <- addToCart("item2", 30)
    } yield total
    println(finalShoppingCart.run(ShoppingCart(Nil, 0)).value)

  }

}

package part3datamanipulation

object Evaluation {

  /*
    Cats make the distinction between
    - evaluating an expression eagerly
    - evaluating lazily and every time you request it
    - evaluating lazily and keeping the value (memoizing)
   */

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    66
  }

  val redoEval = Eval.always {
    println("Computing again!")
    432
  }

  val dealyedVal = Eval.later {
    println("Computing later")
    44
  }

  val composedEvaluation = instantEval.flatMap(v1 => dealyedVal.map(v2 => v1 + v2))

  val anotherComposedEvaluation = for {
    v1 <- instantEval
    v2 <- dealyedVal
  } yield v1 + v2

  // TODO 1:
  val evalEx1 = for {
    a <- dealyedVal
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // "remember" a computed value
  val dontRecompute = redoEval.memoize

  val tutorial = Eval
    .always {
      println("Step 1....")
      "Put the guitar on your lap"
    }
    .map { s1 =>
      println("Step 2")
      s"$s1 then put your left hand on the neck"
    }
    .memoize // remember the value up to this point
    .map { s1And2 =>
      println("Step 3, more complicated")
      s"$s1And2 then with the right hand strike the strings"
    }

  // TODO 2: implement such that defer(Eval.now) does not run the side effect
  def defer[T](eval: => Eval[T]): Eval[T] =
    //Eval.defer(eval)
    Eval.later(()).flatMap(_ => eval)

  // TODO 3: rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] = 
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] = 
    if (list.isEmpty) Eval.now(list)
    else defer(reverseEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    // Computing now!
    // println(evalEx1.value) // computing later, Computing again!, Computing again!, sum
    // println(evalEx1.value) // Computing again!, Computing again!, sum

    // println(dontRecompute.value)
    // println(dontRecompute.value)

    // println(tutorial.value)
    // println(tutorial.value)

    // defer(Eval.now {
    //   println("Now!")
    //   42
    // })

    println(reverseEval((1 to 10000).toList).value)
  }

}

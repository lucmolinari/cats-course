package part3datamanipulation

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object Writers {

  import cats.data.Writer
  // 1 - define them at start
  val aWriter: Writer[List[String], Int] = Writer(List("started"), 45)
  // 2 - manipulate them with pure FP
  val anIncreasedWriter                  = aWriter.map(_ + 1)                                     // value increases, logs stay the same
  val aLogsWriter                        = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth                    = aWriter.bimap(_ :+ "found something", _ + 1)           // both changes
  val aWriterWithBoth2                   = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something", value + 1)
  }

  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)

  import cats.instances.vector._ // import a Semigroup[Vector]
  import cats.instances.list._   // import a Monoid[List]

  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  // 3 - dump wither the value or the logs
  val desiredValue = aWriter.value
  val logs         = aWriter.written
  val (l, v)       = aWriter.run

  // TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("Starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting"), 0)
    else {
      countAndLog(n - 1).bimap(_ :+ n.toString(), _ + 1)
    }
  }

  // Benefit #1: we work with pure FP

  // TODO 2: rewrite this method with Writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum of ${n - 1} = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithWriters(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else
      for {
        _        <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithWriters(n - 1)
        _        <- Writer(Vector(s"Computed sum of ${n - 1} = $lowerSum"), n)
      } yield lowerSum + n
  }

  // Benefit #2: Writers can keep logs isolated in each Thread

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    // countAndSay(10)
    // println(countAndLog(10).run)
    println("==> naiveSum")
    // val res = naiveSum(4)
    // println(res)
    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    println("==> sumWithWriters")
    val sumFuture1 = Future(sumWithWriters(100))
    val sumFuture2 = Future(sumWithWriters(100))
    val logs1      = sumFuture1.map(_.written)
    val logs2      = sumFuture2.map(_.written)
    // println(sumWithWriters(4).run)
  }

}

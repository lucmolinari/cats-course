package part2abstractmath

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import scala.concurrent.Future

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // option transformer
  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List]
  import cats.instances.future._

  val listOfNumbersOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char]   = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char   <- listOfCharOptions
    number <- listOfNumbersOptions
  } yield (number, char)

  // either transformer
  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int]    = EitherT(
    List(
      Left("Something wrong"),
      Right(43),
      Right(2)
    )
  )
  implicit val ec: ExecutionContext                = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45))

  /*
    TODO: exercise
    We have a multi-machine cluster for your business which will received a traffic surge following a media appearance.
    We measure bandwidth in units.
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths > 250.
   */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(serverName: String): AsyncResponse[Int] = bandwidths.get(serverName) match {
    case None    => EitherT.left(Future(s"Server $serverName not found"))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO 1
  // hint: call getBandwidth twice and combine the results
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    bd1 <- getBandwidth(s1)
    bd2 <- getBandwidth(s2)
  } yield (bd1 + bd2) > 250

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(desc)   => Left(desc)
      case Right(false) => Left(s"$s1 and $s2 capacity can't withstand the surge")
      case Right(true)  => Right(s"$s1 and $s2 can withstand the surge")
    }

  def main(args: Array[String]): Unit = {
    // println(listOfTuples.value)
    printValue(generateTrafficSpikeReport("server1.rockthejvm.com", "server3.rockthejvm.com").value) // can't withstand
    printValue(generateTrafficSpikeReport("server1.rockthejvm.com", "server2.rockthejvm.com").value) // can withstand
    printValue(generateTrafficSpikeReport("server1.rockthejvm.com", "server4.rockthejvm.com").value) // server not found
    // executor.shutdown()
  }

  def printValue(f: Future[Either[String, String]]): Unit = f.foreach(println)

}

package part2abstractmath

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List] // fetch implicit Monad[List

  // either is also a Monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T]   = Either[Throwable, T]
  import cats.instances.either._
  val loadingMonad    = Monad[LoadingOr]
  val anEither        = loadingMonad.pure(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading.."))

  // imaginary online store
  case class OrderStatus(oderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.oderId > 1000) {
      Left("Not available yet")
    } else {
      Right("Amsterdam, NL")
    }

  val orderId                          = 457L
  val orderLocation: LoadingOr[String] = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)
  // use extension methods (not needed, since Either already provides them)
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location    <- trackLocation(orderStatus)
  } yield location

  //TODO: the service layer API of a web app
  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }
  // DO NOT CHANGE THE CODE

  /*
    Requirements:
    - if the host and port are found in the cfg map, then we'll return a M containing a connection with those values
      otherwise the method will fail, acording to the logic of the type M
      (for Try -> Failure, for Option -> None,...)
    - the issueRequest method returns a M containing the String: "request (payload) has been accepted", if the payload is less
      than 20 chars, otherwise it'll fail according to the logic of M

    //TODO: provide a real impl of HttpService using Try, Option, Either..
   */
  object EitherHttpService extends HttpService[LoadingOr] {

    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] = {
      (cfg.get("host"), cfg.get("port")) match {
        case (Some(h), Some(p)) => Right(Connection(h, p))
        case _                  => Left("Either host or port are missing")
      }
    }

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] = {
      if (payload.length() < 20) {
        Right(s"request ($payload) has been accepted")
      } else {
        Left("Error while issuing request")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    // val responseLoadingOr = EitherHttpService
    //   .getConnection(config)
    //   .flatMap(c => EitherHttpService.issueRequest(c, "Hello, Service"))
    val responseLoadingOr = for {
      conn     <- EitherHttpService.getConnection(config)
      response <- EitherHttpService.issueRequest(conn, "Hello, Service")
    } yield (response)
    println(responseLoadingOr)
  }

}

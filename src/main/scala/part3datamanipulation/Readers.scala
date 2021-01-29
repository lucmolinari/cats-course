package part3datamanipulation

object Readers {

  /*
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */
  case class Configuration(
    dbUsername: String,
    dbPassword: String,
    host: String,
    port: Int,
    nThreads: Int,
    emailReplyTo: String
  )

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String  = "dispatched" // select ... and return the status
    def getLastOrderId(username: String): Long = 22
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  val config = Configuration("root", "pass", "localhost", 8080, 3, "root@localhost.com")

  // cats Reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn                                        = dbReader.run(config)

  // Reader[I, O]
  val danielsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val danielsOrderStatus                                      = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    // usersLastOrderIdReader.run(config)
    usersOrderFor.run(config)
  }

  /*
    Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure will be manipulated later
    3. you can then map & flatMap the reader to produce derived information
    4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  // TODO 1
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String): String = {
    val emailReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

    val sendEmailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      email       <- emailReader.map(_.sendEmail(userEmail, s"Your last order has the status: $orderStatus"))
    } yield email
    sendEmailReader.run(config)
    // fetch status last order
    // email them with the Email Service: Your last order has the status: (status)
  }

  // TODO 2: what programming pattern do Reader remind you of?
  // Dependency injection

  def main(args: Array[String]): Unit = {
    // println(getLastOrderStatus("root"))
    println(emailUser("customer1", "customer1@localhost"))
  }

}

package part1intro

object Implicits {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my nsme is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  // val imppersonableString = new ImpersonableString("Peter")
  // imppersonableString.greet

  val greeting = "Peter".greet

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount                  = 10
  val incremented2                            = increment(2)

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer = new JSONSerializer[Person] {

    def toJson(person: Person): String =
      s"""
        | {"name-person:" "${person.name}"}
        | """.stripMargin
  }
  //implicit argument is used TO PROVE EXISTENCE of a type!

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {

    def toJson(value: T): String =
      s"""
        |"{${value.productElementName(0)}" : "${value.productElement(0)}}"
        |""".stripMargin.trim
  }
  case class Cat(catName: String)

  // can also be used as conversion (DISCOURAGED!)
  

  def main(args: Array[String]): Unit = {
    val personsJson = listToJson(List(Person("Alice"), Person("Bob")))
    println(personsJson)

    val catsJson = listToJson(List(Cat("Cat 1"), Cat("Cat 2")))
    println(catsJson)

  }

}

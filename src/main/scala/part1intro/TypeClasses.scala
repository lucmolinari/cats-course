package part1intro

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - create implicit type class Instances
  implicit object StringSerializer extends JSONSerializer[String] {
    def toJson(value: String): String = s""""$value""""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {

    def toJson(person: Person): String =
      s"""
         | {"name" : "${person.name}", "age" : ${person.age}}
         | """.stripMargin.trim
  }

  // part 3 - offer some API
  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(serializer.toJson).mkString("[\n ", ",\n ", "\n]")

  // part 4 - expanding the existing types via extension methods
  object JSONSyntax {

    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    // println(convertListToJson(List(Person("Joe", 36), Person("Alice", 34))))
    import JSONSyntax._
    //println(List(Person("Joe", 36), Person("Alice", 34)).toJson)
    println(Person("Joe", 36).toJson)
  }

}

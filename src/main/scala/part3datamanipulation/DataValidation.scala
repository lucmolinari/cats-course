package part3datamanipulation

import scala.annotation.tailrec
import cats.kernel.Semigroup
import scala.util.Try

object DataValidation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int]    = Validated.valid(1)                        // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value
  val aTest: Validated[String, Int]          = Validated.cond(42 > 39, 1, "Something went wrong")

  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    tailrecPrime(Math.abs(n / 2))
  }

  // TODO: user Either
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String]  = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isTooBig: List[String]   = if (n <= 100) List() else List("Number must be <= 100")
    val isNotPrime: List[String] = if (testPrime(n)) List() else List("Number must be a prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isTooBig ++ isNotPrime)
  }

  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance(Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated
      .cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be <= 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // transforms
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.toUpperCase())
  aValidValue.bimap(_.length, _ + 1)
  // interporate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Some error"))
  val tryToValidated: Validated[Throwable, Int]       = Validated.fromTry(Try("something".toInt))

  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // TODO 2 - form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"Field $fieldName must be specified"))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"Field $fieldName must not be blank"))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List(s"Email is invalid"))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List(s"Password must have at least 10 chars"))

    import cats.instances.string._

    /*
      - fields: name, email, password
      - rules:
        - name, email, password must be specified
        - name must not be blank
        - email must have "@"
        - passwpord must have >= 10 char
     */
    def validateForm(form: Map[String, String]): FormValidation[String] =
      getValue(form, "Name")
        .andThen(nonBlank(_, "Name"))
        .combine(getValue(form, "Email").andThen(emailProperForm))
        .combine(getValue(form, "Password").andThen(passwordCheck))
        .map(_ => "Success")

  }

  import cats.syntax.validated._
  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int]                   = "Error".invalid[Int]

  def main(args: Array[String]): Unit = {
    val form = Map("Name" -> "Luciano", "Email" -> "luciano@email.com", "Password" -> "1234567891")
    println(FormValidation.validateForm(form))
  }

}

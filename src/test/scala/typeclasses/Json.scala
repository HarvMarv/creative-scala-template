import typeclasses.JsonWriter._
import typeclasses.JsonWriter

class JsonSuite extends munit.FunSuite {
  test("Json") {
    import typeclasses.JsonWriter._

    

    val number: Double = 5.0
    val long: Long = 6L

    toJson(number)
    // toJson(long)
    // long.toJson

    // long.toJson
    print(number.toJson)
    // https://books.underscore.io/essential-scala/essential-scala.html#enriched-interfaces
  }
}

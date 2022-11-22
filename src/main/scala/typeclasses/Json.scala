package typeclasses

// A representation of Json as an algebraic data type
sealed trait Json
case object JNull extends Json
final case class JBoolean(value: Boolean) extends Json
final case class JString(value: String) extends Json
final case class JNumber(value: Double) extends Json
final case class JObject(value: Map[String, Json]) extends Json
final case class JArray(value: List[Json]) extends Json

// Implement a type class for writing a type A to Json
trait JsonWriter[A] {
  def toJson(a: A): Json
}
// What language feature implements a type class in Scala?

final case class Product(name: String, description: String, price: Double)
final case class Order(items: List[Product])
// Implement type class instances to write Product and Order to Json. The Json
// representation is not particularly important but at least make it sensible.
//
// What Scala language feature implements a type class instance?
object JsonWriter {

  def toJson[A](a: A)(implicit writer: JsonWriter[A]): Json =
    writer.toJson(a)

  implicit class JsonWriterOps[A](a: A) {
    def toJson(implicit writer: JsonWriter[A]): Json = writer.toJson(a)
  }

  implicit val stringJsonWriter: JsonWriter[String] = new JsonWriter[String] {
    def toJson(a: String): Json = JString(a)
  }

  implicit val doubleJsonWriter: JsonWriter[Double] = new JsonWriter[Double] {
    def toJson(a: Double): Json = JNumber(a)
  }

  implicit val booleanJsonWriter: JsonWriter[Boolean] = new JsonWriter[Boolean] {
    def toJson(a: Boolean): Json = JBoolean(a)
  }

  // If you can write a type A to Json, what does that mean in terms of type class instances?
  //
  // [A: JsonWriter] is called a context bound
  // It's the same thing as
  // (a: A)(implicit jw: JsonWriter[A])
  // except the implicit parameter doesn't have a name in the scope of the method body

  // where can we introduce type parameters?
  // - declaring classes / traits (e.g. sealed trait Expression[A])
  // - declaring methods (e.g. def map[B](f: A => B))
  implicit def arrayJsonWriter[A](implicit writer: JsonWriter[A]): JsonWriter[List[A]] = new JsonWriter[List[A]] {
    def toJson(list: List[A]): Json = JArray(list.map(writer.toJson(_)))
  }

  implicit def objectJsonWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Map[String, A]] = new JsonWriter[Map[String, A]] {
    def toJson(obj: Map[String, A]): Json = JObject(obj.map { case (string, a) => string -> writer.toJson(a) })
  }

  implicit val productJsonWriter: JsonWriter[Product] = new JsonWriter[Product] {
    def toJson(a: Product): Json = JObject(Map(
      "name" -> a.name.toJson,
      "description" -> a.description.toJson,
      "price" -> a.price.toJson
    ))
  }

  implicit val orderJsonWriter: JsonWriter[Order] = new JsonWriter[Order] {
    def toJson(order: Order): Json = arrayJsonWriter[Product].toJson(order.items)
  }
}

// Implement syntax `toJson` that converts any type to Json that has a Json writer instance.

// Implement a type class instance that writes a List of any type that can be be
// written to Json.

// Implement a type class instance that writes an Option of any type that can be be
// written to Json.

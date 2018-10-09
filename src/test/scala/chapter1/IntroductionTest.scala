package chapter1

import org.scalatest._

class JsonSpec extends FlatSpec with Matchers {
  "Json" should "json" in {
    import JsonWriterInstances._
    val json =Json.toJson(Person("Dave", "dave@example.com"))
    json shouldBe JsObject(Map(
          "name" -> JsString("Dave"),
          "email" -> JsString("dave@example.com")
        ))
  }

  "Json2" should "json" in {
    import JsonWriterInstances._
    import JsonSyntax._

    val json = Person("Dave", "dave@example.com").toJson
    json shouldBe JsObject(Map(
          "name" -> JsString("Dave"),
          "email" -> JsString("dave@example.com")
        ))
  }

  "Implicitly" should "implicitly" in {
    import JsonWriterInstances._

    implicitly[JsonWriter[String]] shouldBe stringWriter
  }

  "JsonOption" should "json" in {
    import JsonWriterInstances._
    import JsonSyntax._

    val json = Option(Person("Dave", "dave@example.com")).toJson
    json shouldBe JsObject(Map(
          "name" -> JsString("Dave"),
          "email" -> JsString("dave@example.com")
        ))
  }

  "JsonOptionString" should "json" in {
    import JsonWriterInstances._

    val json = Json.toJson(Option("A string"))
    json shouldBe JsString("A string")
  }
}


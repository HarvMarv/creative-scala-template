class MaybeSuite extends munit.FunSuite {
  test("isDefined") {
    val thing = Just(5)
    assertEquals(thing.isDefined, true)
  }

  test("doesnt isDefined") {
    val thing = Empty[String]()
    assertEquals(thing.isDefined, false)
  }

  test("!isEmpty") {
    val thing = Just(5)
    assertEquals(thing.isEmpty, false)
  }

  test("isEmpty") {
    val thing = Empty[String]()
    assertEquals(thing.isEmpty, true)
  }

  test("isDefined string") {
    val thing = Just("hello")
    assertEquals(thing.isDefined, true)
  }

  test("map some") {
    val thing = Just("hello")
    val f: String => String = (a: String) => a + " world"
    val otherThing = thing.map(f)
    assertEquals(otherThing, Just("hello world"))
  }
  test("map None") {
    val thing: Maybe[String] = Empty()
    val otherThing = thing.map(_ + " world")
    assertEquals(otherThing, Empty[String]())
    println(otherThing)
  }

  test("getOrElse") {
    val thing: Maybe[Long] = Empty()
    val default = thing.getOrElse(System.currentTimeMillis)
    println(default)
    // assertEquals(thing, Empty[Long]())
  }

  test("getOrElse with function") {
    val thing: Maybe[Long] = Empty()
    val f: Long => Long = (x: Long) => 5
    val default = thing.getOrElse(System.currentTimeMillis())
    assertEquals(thing, Empty[Long]())
  }

  class Animal
  case class Dog() extends Animal
  case class Cat() extends Animal

  test("getOrElse to Int") {
    val animal = new Animal
    val dog = Dog()
    val notDog = Empty[Dog]()
    val justDog: Maybe[Animal] = Just(dog)
    val one = justDog.getOrElse(animal)
    assertEquals(one, dog)

    val other = notDog.getOrElse(animal)
    assertEquals(other, animal)

    val notWorks = notDog.getOrElse(5)
    assertEquals(notWorks, 5)
  }

  // test("orElse") {
  //   val thing: Maybe[String] = Empty[String]()
  //   val alternative = thing.orElse(Just("hello"))
  //   assertEquals(alternative, Just("hello"))
  // }
  //   test("not orElse") {
  //   val thing: Maybe[String] = Just("cheese")
  //   val orElse = thing.orElse(Just("hello"))
  //   assertEquals(orElse, Just("cheese"))
  // }
}

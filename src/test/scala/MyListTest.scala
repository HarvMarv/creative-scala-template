import GenericTypes._
class MyListSuite extends munit.FunSuite {
  // Basics
  val list = Pair(1, Pair(1, End()))
  test("length") {
    assertEquals(list.length, 2)
  }

  test("contains") {
    assertEquals(list.contains(1), true)
    assertEquals(list.contains(2), false)
  }

  test("find") {
    assertEquals(list.find((x) => x >= 1), Some(1))
    assertEquals(list.find((x) => x >= 100), None)
  }

  test("append") {
    val newList = list.append(list)

    assertEquals(list.length, 2)

    assertEquals(newList.length, 4)
  }

  def double(item: Int): Int = {
    item * 2
  }

  test("map") {
    val myList = Pair(1, Pair(2, End()))
    val doubledList = myList.map(double)
    assertEquals(doubledList.length, 2)

    assertEquals(doubledList.find((x) => x < 2), None)
  }

  def flatMapFunc(item: Int): MyList[Int] = {
    Pair(item, Pair(item * 2, End()))
  }

  test("flatMap") {
    val myList = Pair(1, Pair(2, End()))
    val flatList = myList.flatMap(flatMapFunc)
    assertEquals(flatList.length, 4)
  }

  test("push") {
    val myList = Pair(2, End())
    val anotherList = myList.push(3)
    assertEquals(anotherList.length, 2)
  }

  test("fold") {
    val myList = Pair(5, Pair(10, Pair(2, End())))
    def add: (Int, Int) => Int = {
      _ + _
    }
    assertEquals(myList.fold(0, add), 17)
  }

}

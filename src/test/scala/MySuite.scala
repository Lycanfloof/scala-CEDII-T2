// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  import T2.*

  test("checkInList:") {
    assertEquals(checkInList(List[String]("Hello", "World!"), "World!"), true)
    assertEquals(checkInList(List[String]("Hello", "World!"), ":)"), false)
  }

  test("replaceInList:") {
    assertEquals(replaceInList(List[Int](0, 1, 0, 1, 0, 1, 1, 1, 0, 1), 0, 5), List[Int](5, 1, 5, 1, 5, 1, 1, 1, 5, 1))
  }

  test("returnOddFromList:") {
    assertEquals(returnOddFromList(List[Int](2, 1, 3, 4, 5, 5, 6, 1)), List[Int](1, 3, 5, 5, 1))
  }

  test("sumMultiplesOfFive:") {
    assertEquals(sumMultiplesOfFive(List[Int](4, 1, 5, 20, 3, 7, 8, 10)), 35)
  }
}

import org.scalatest.FunSuite

class TestHomework extends FunSuite {
  test("FirstHomework.gcd") {
    assert(FirstHomework.gcd(0,8) === 8)
    assert(FirstHomework.gcd(8,0) === 8)
  }
  test("FirstHomework.lcm") {
    assert(FirstHomework.lcm(0,8) === 0)
    assert(FirstHomework.lcm(8,0) === 0)
  }
}

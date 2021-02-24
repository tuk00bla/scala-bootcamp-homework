import TypeClassTask._
import org.scalatest.funsuite.AnyFunSuite

class TCTaskTest extends AnyFunSuite {


  test("Hash for string works fine") {
    implicit val hashcodeString: HashCode[String] = _.hashCode
    assert("Test works fine".hash == -755939386)
  }
}
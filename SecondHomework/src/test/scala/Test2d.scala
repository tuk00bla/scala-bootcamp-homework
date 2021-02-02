import org.scalatest._

class Test2d extends FunSuite {
  test("Point should move in") {
    val point = Point(-1.0, 4.0)
    val movedPoint = point.move(2.0, 3.0)
    assert(movedPoint.x === 1.0, movedPoint.y === 7.0)
  }
  test("Circle should move in") {
    val circle = Circle(0, 0, 2.0)
    val movedCircle = circle.move(2.0, 3.0)
    assert(movedCircle.x === 2.0, movedCircle.y ===3.0)
  }
  test("Circle should report its area") {
    val rad = 3.0
    val circle = Circle(0, 0, rad)
    val expArea = Math.PI * Math.pow(rad, 2)
    assert(circle.area === expArea)

  }
  test("Rectangle should move in") {
    val rectangle = Rectangle(-1.0, 4.0, 3, 6)
    val movedRectangle = rectangle.move(2.0, 3.0)
    assert(movedRectangle.x === 1.0, movedRectangle.y === 7.0)
  }
  test("Rectangle should report its area") {
    val rectangle = Rectangle(0, 0, 3.0, 4.0)
    val expArea = 3.0 * 4.0
    assert(rectangle.area === expArea)

  }
}

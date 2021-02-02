import org.scalatest._

class Test3d extends FunSuite {
  test("Sphere should move in") {
    val sphere = Sphere(0.0, 0.0, 3.0, 4.0)
    val movedSphere = sphere.move(2.0, 3.0, 0.0)
    val expectedSphere = Sphere(2.0, 3.0, 3.0, 4.0)
    assert(expectedSphere === movedSphere)
  }
  test("Sphere should report area") {
    val rad = 3.5
    val sphere = Sphere(0.0, 0.0, 0.0, rad)
    val expArea = 4 * math.Pi * math.pow(rad, 2)
    assert(expArea === sphere.surfaceArea())
  }
  test("Sphere should report volume") {
    val rad = 5.0
    val sphere = Sphere(0.0, 0.0, 0.0, rad)
    val expVol = (4 * math.Pi * math.pow(rad, 3)) / 3
    assert(expVol === sphere.volume())
  }
  test("Cube should move in") {
    val cube = Cube(0.0, 0.0, 3.0, 4.0)
    val movedCube = cube.move(2.0, 3.0, 0.0)
    val expectedCube = Cube(2.0, 3.0, 3.0, 4.0)
    assert(expectedCube === movedCube)
  }
  test("Cube should report area") {
    val length = 4.5
    val cube = Cube(0.0, 0.0, 0.0, length)
    val expArea  = 6 * Math.pow(length, 2)
    assert(expArea === cube.surfaceArea())
  }
  test("Cube should report volume") {
    val length = 4.5
    val cube = Cube(0.0, 0.0, 0.0, length)
    val expVol = Math.pow(length, 3)
    assert(expVol === cube.volume())
  }
  test("Cuboid should move in") {
    val cuboid = Cuboid(0.0, 0.0, 0.0, 4.0, 6.0, 5.0)
    val movedCuboid = cuboid.move(3.0, 4.0, 0.0)
    val expectedCuboid = Cuboid(3.0, 4.0, 0.0, 4.0, 6.0, 5.0)
    assert(expectedCuboid === movedCuboid)
  }
  test("Cuboid should report area") {
    val width = 3.5
    val length = 5.1
    val height = 4.2
    val cuboid = Cuboid(0.0, 0.0, 0.0, width, height, length)
    val expArea = 2 * (length * width + length * height + width * height)
    assert(expArea === cuboid.surfaceArea())
  }
  test("Cuboid should report volume") {
    val width = 3.5
    val length = 5.1
    val height = 4.2
    val cuboid = Cuboid(0.0, 0.0, 0.0, width, height, length)
    val expVol = length * width * height
    assert(expVol === cuboid.volume())
  }
}


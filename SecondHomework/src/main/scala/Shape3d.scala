import scala.math

sealed trait Shape3d extends Located3d with Bounded3d with Movable3d{
  def surfaceArea() : Double
  def volume(): Double
}

sealed trait Located3d
{
  def x: Double
  def y: Double
  def z: Double
}

sealed trait Bounded3d
{
  def minX: Double
  def minY: Double
  def minZ: Double

  def maxX: Double
  def maxY: Double
  def maxZ: Double
}

sealed trait Movable3d
{
  def move(toX: Double, toY: Double, toZ: Double):Shape3d
}

final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, rad: Double) extends Shape3d {

  override def x: Double = centerX
  override def y: Double = centerY
  override def z: Double = centerZ

  override def minX : Double = x - rad
  override def minY: Double = y - rad
  override def minZ: Double = z - rad

  override def maxX: Double = x + rad
  override def maxY: Double = y - rad
  override def maxZ: Double = z + rad


  override def move(toX: Double, toY: Double, toZ: Double): Sphere =
    Sphere(x + toX, y + toY, z + toZ, rad)

  override def surfaceArea(): Double = 4 * math.Pi * math.pow(rad, 2)
  override def volume(): Double = (4 * math.Pi * math.pow(rad, 3)) / 3
}

final case class Cube(x: Double, y: Double, z: Double, length: Double) extends Shape3d {

  override def minX: Double = x
  override def minY: Double = y
  override def minZ: Double = z

  override def maxX: Double = x + length
  override def maxY: Double = y + length
  override def maxZ: Double = z + length

  override def move(toX: Double, toY: Double, toZ: Double): Cube = Cube(x + toX, y + toY, z + toZ, length)

  override def surfaceArea: Double = 6 * Math.pow(length, 2)
  override def volume: Double = Math.pow(length, 3)
}

final case class Cuboid(x: Double, y: Double, z: Double, width: Double, height: Double, length: Double) extends Shape3d{

  override def minX: Double = x
  override def minZ: Double = z
  override def minY: Double = y

  override def maxX: Double = x + width
  override def maxY: Double = y + height
  override def maxZ: Double = z + length

  override def move(toX: Double, toY: Double, toZ: Double): Cuboid =
    Cuboid(x + toX, y + toY, z + toZ, width, height, length)

  override def surfaceArea: Double = 2 * (length * width + length * height + width * height)
  override def volume: Double = length * width * height
  }

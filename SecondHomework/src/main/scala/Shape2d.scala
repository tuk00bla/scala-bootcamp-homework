import scala.math

sealed trait Shape2d extends Located2D with Bounded2D with Movable2D{
  def area : Double
}

sealed trait Located2D
{
  def x: Double
  def y: Double
}

sealed trait Bounded2D
{
  def minX: Double
  def minY: Double

  def maxX: Double
  def maxY: Double
}

sealed trait Movable2D
{
  def move(toX: Double, toY: Double):Shape2d
}

final case class Point(x: Double, y: Double) extends Shape2d {
  override def minX: Double = x
  override def minY: Double = y

  override def maxX: Double = x
  override def maxY: Double = y

  override def move(toX: Double, toY: Double): Point = Point(x + toX, y + toY)

  override def area: Double = throw new UnsupportedOperationException("Point does not have area")
}
final case class Circle (centerX: Double, centerY: Double, rad: Double) extends Shape2d
{
  override val x: Double = centerX
  override val y: Double = centerY

  override def minX : Double = x - rad
  override def minY: Double = y - rad

  override def maxX: Double = x + rad
  override def maxY: Double = y - rad

  override def move(toX: Double, toY: Double): Circle = Circle(x + toX, y + toY, rad)

  override def area: Double = Math.PI * Math.pow(rad, 2)
}

final case class Rectangle (x: Double, y: Double, width: Double, height: Double) extends Shape2d
{
  override def minX : Double = x
  override def minY: Double = y

  override def maxX: Double = x + width
  override def maxY: Double = y + height

  override def move(toX: Double, toY: Double): Rectangle = Rectangle(x + toX, y + toY, width + toX, height + toY)

  override def area: Double = (maxX - minX) * (maxY - minY)
}

object FirstHomework extends App{
  def gcd(a:Int, b:Int):Int = {
    if (b == 0) a
    else {
      gcd (b, a % b)
    }
  }
  def lcm(a: Int, b: Int): Int = if (b == 0) 0 else Math.abs(a * b) / gcd(a, b)
}
object TypeClassTask extends App {

  trait HashCode[T] {
    def hash(t: T): Int
  }

  object HashCode {
    def apply[A: HashCode]: HashCode[A] = implicitly[HashCode[A]]
  }

  implicit class HashCodeSyntax[B: HashCode](x: B) {
    def hash: Int = HashCode[B].hash(x)
  }
  implicit val hashcodeString: HashCode[String] = _.hashCode
}
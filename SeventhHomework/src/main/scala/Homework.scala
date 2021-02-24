object Task1 extends App {
  // TODO: create Ordering instance for Money
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

}

object Task2 {

  trait Show[T] {
    def show(entity: T): String
  }

  object Show {
    def apply[A](implicit instance: Show[A]): Show[A] = instance
  }

  implicit class ShowSyntax[B](x: B) {
    def show(implicit instance: Show[B]): String = instance.show(x)
  }

  implicit object ShowUser extends Show[User] {
    override def show(entity: User): String = s"User with name: ${entity.name} and id: ${entity.id}"
  }

  final case class User(id: String, name: String)
  // TODO: create Show instance for User
  // TODO: create syntax for Show so i can do User("1", "Oleg").show

  User("1", "Oleg").show

}

object Task3 {

  type Error = String

  trait Parse[T] {
    def parse(entity: String): Either[Error, T]
  }
  object Parse {
    def apply[A](implicit instance: Parse[A]): Parse[A] = instance
  }

  implicit class ParseSyntax(x: String) {
    def parse[B](implicit instance: Parse[B]): Either[Error, B] = instance.parse(x)
  }

  implicit val parseUser: Parse[User] = (obj: String) => {
    obj.split(",").toList match {
      case id :: name :: Nil => Right(User(id, name))
      case _ => Left(s"Wrong entry: $obj, use format: id,name")
    }
  }

  final case class User(id: String, name: String)
  // TODO: create Parse instance for User
  // TODO: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)

  "lalalal".parse[User]
  "3,Nick".parse[User]
}

object Task4 extends App {

  // Design a typesafe equals so i can do a === b,
  // but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`
  trait Eq[T] {
    def equals(a: T, b: T): Boolean
  }

  object Eq {
    def apply[A](implicit instance: Eq[A]): Eq[A] = instance
  }

  implicit class EqSyntax[B](a: B) {
    def ===(b: B)(implicit instance: Eq[B]): Boolean =
      instance.equals(a, b)
  }

  implicit val userEq: Eq[User] = (a: User, b: User) => a.id.equals(b.id)

  final case class User(id: String, name: String)

  User("1", "Nick") === User("2", "Nick") // false
  User("1", "Jack") === User("1", "Jack") // true
  //  User("1", "Rose") === "Rose" // compile error
}

object AdvancedHomework {
  // TODO: create a typeclass for flatMap method
}

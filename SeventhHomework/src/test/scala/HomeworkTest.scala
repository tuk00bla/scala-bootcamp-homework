import Task2._
import Task3._
import org.scalatest.funsuite.AnyFunSuite

  class HomeworkTest extends AnyFunSuite {

      test("[Task2] User info shows fine") {

        val user = Task2.User("1", "Oleg")
        assert(user.show == s"User with name: ${user.name} and id: ${user.id}")

      }


      test("[Task3] Parse works fine") {

        assert("1,John".parse[Task3.User] == Right(Task3.User("1", "John")))

      }

    test("[Task4] Equals works fine") {

      val a = Task4.User("1", "Nick")
      val b = Task4.User("1", "Nick")
      assert(a.equals(b))
    }
  }

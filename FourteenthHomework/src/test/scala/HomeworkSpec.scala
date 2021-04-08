import FourteenthHomework._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class HomeworkSpec extends AnyFreeSpec {
  import HomeworkSpec._

  "Effect should be delayed unless unsafeRunSync is called" - {

    "IO.apply" in {
      val f = MockEffect()
      val iof = IO(f("foo"))

      assert(f.notCalled)
      iof.unsafeRunSync()
      assert(f.calledOnce)
    }

    "IO.pure injects value" in {
      val ioa = IO.pure("foo")
      ioa.unsafeRunSync() should equal("foo")
    }

    "IO.delay" in {
      val f = MockEffect()
      val iof = IO.delay(f("foo"))

      assert(f.notCalled)
      iof.unsafeRunSync()
      assert(f.calledOnce)
    }

    "IO.suspend" in {
      val f = MockEffect()
      val iof = IO.delay(f("foo"))
      val iog = IO.suspend(iof)

      assert(f.notCalled)
      iog.unsafeRunSync()
      assert(f.calledOnce)
    }
  }

  "IO.unit injects unit" in {
    IO.unit.unsafeRunSync() should equal(())
  }

  "IO::flatMap composes IO" in {
    val f = MockEffect()
    val iof = IO(f("foo")).flatMap(s => IO(f(s + "bar")))

    assert(f.notCalled)
    iof.unsafeRunSync()
    assert(f.calledExact(Seq("foo", "foobar")))
  }

  "IO::map applies function to IO value" in {
    val f = MockEffect()
    val iof = IO(f("foo")).map(_ + "bar")
    assert(f.notCalled)
    iof.unsafeRunSync() should equal("foobar")
    assert(f.calledExact(Seq("foo")))
  }

  "Error handling" - {

    "IO::handleErrorWith maps errors" - {
      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.handleErrorWith(_ => IO.pure("handled")).unsafeRunSync() should equal("success")
        assert(f.calledOnceWith("success"))
      }
    }

    "IO::attempt turns errors to Either" - {
      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.attempt.unsafeRunSync() should equal(Right("success"))
        assert(f.calledOnceWith("success"))
      }
    }

    "IO::option turns errors to None" - {
      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.option.unsafeRunSync() should equal(Some("success"))
        assert(f.calledOnceWith("success"))
      }
    }

    "IO::redeem maps errors" - {
      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.redeem(_ => "foo", identity).unsafeRunSync() should equal("success")
        assert(f.calledOnceWith("success"))
      }
    }

    "IO::redeemWith maps errors" - {
      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.redeemWith(_ => IO.pure("foo"), IO.pure).unsafeRunSync() should equal("success")
        assert(f.calledOnceWith("success"))
      }
    }
  }

}

object HomeworkSpec {
  class MockEffect {
    var history: ListBuffer[String] = mutable.ListBuffer[String]()

    def apply(param: String): String = {
      history += param
      param
    }

    def calledOnce = history.length == 1
    def called = history.nonEmpty
    def notCalled = !called
    def calledOnceWith(param: String) = calledOnce && history.head == param
    def calledWith(param: String) = history.count(_ == param) > 0
    def calledExact(params: Seq[String]) =
      params.length == history.length && params.zip(history).forall {
        case (left, right) => left == right
      }
  }

  object MockEffect {
    def apply() = new MockEffect()
  }
}
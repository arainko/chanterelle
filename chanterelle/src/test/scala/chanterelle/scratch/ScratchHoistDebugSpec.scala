package chanterelle

import chanterelle.OpaqueFixtures.*
import chanterelle.internal.Logger
import chanterelle.interop.Mappable

final class NonOpqBox[+A](val value: A)

object NonOpqBox {

  given Mappable[NonOpqBox] with {
    def map[A, B](fa: NonOpqBox[A], f: A => B): NonOpqBox[B] = NonOpqBox(f(fa.value))
  }
}

class ScratchHoistDebugSpec extends ChanterelleSuite {
  test("prod the crash") {
    Mode.FailFast.option {
      val ok = (field = Some(NonOpqBox((inner = Some(1)))))
      Logger.locally:
        val actualOk = ok.transform(
          // _.update(_.field.some.element.inner.some)(_ + 1)
          _.hoist(_.field.some.element.inner)
        )
        // assertEquals(actualOk, Some((field = OpqBox((inner = 1), "t"))))
    }
  }
}

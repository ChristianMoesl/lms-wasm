package lms.wasm

import lms.TutorialFunSuite
import lms.core.virtualize
import lms.macros.SourceContext

class ControlFlowTest extends TutorialFunSuite {
  val under = "backend/control_flow/"

  test("switch") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        switch(arg, Some(() => printf("3")))(
          (Seq(1), x => printf("1")),
          (Seq(2, 3), x => printf("2"))
        )
      }
    }

    val src = driver.watCode
    checkSnippet("switch", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(1)
      driver.eval(2)
      driver.eval(3)
      driver.eval(100)
    })
  }
}

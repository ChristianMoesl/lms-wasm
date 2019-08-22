package lms.wasm

import lms.TutorialFunSuite
import lms.core.virtualize
import lms.macros.SourceContext

class BooleanOpsTest extends TutorialFunSuite {
  val under = "backend/boolean/"

  test("and") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x: Rep[Boolean] = (arg > 1) && { printf("%d\n", 10); arg } > 2
        printf("%d\n", x)
      }
    }

    val src = driver.watCode
    checkSnippet("and", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(1)
      driver.eval(2)
      driver.eval(3)
    })
  }

  test("or") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x: Rep[Boolean] = { printf("%d\n", 20); arg > 1 } || { printf("%d\n", 10); arg } > 2
        printf("%d\n", x)
      }
    }

    val src = driver.watCode
    checkSnippet("or", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(1)
      driver.eval(2)
      driver.eval(3)
    })
  }

  test("not") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = !(arg > 1)
        printf("%d\n", x)
      }
    }

    val src = driver.watCode
    checkSnippet("not", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(1)
      driver.eval(2)
    })
  }

  test("precedence") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x: Rep[Boolean] = arg > 2 && arg > 1 || arg > 1
        printf("%d\n", x)
      }
    }

    val src = driver.watCode
    checkSnippet("precedence", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(2)
    })
  }
}

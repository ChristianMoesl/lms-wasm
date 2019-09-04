package lms.wasm

import lms.TutorialFunSuite
import lms.core.virtualize
import lms.macros.SourceContext

class EnvironmentTest extends TutorialFunSuite {
  val under = "backend/environment/"

  test("print_integer") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        println(arg)
      }
    }

    val src = driver.watCode
    checkOut("print_integer", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(1)
    })
  }

  test("print_long") {
    // long type is not supported for WASM in V8
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        println(arg.toLong)
      }
    }

    val src = driver.watCode
    checkOut("print_long", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(1)
    })
  }

  test("print_float") {
    val driver = new DslDriverWasm[Float,Unit] {
      @virtualize
      def snippet(arg: Rep[Float]) = {
        println(arg)
      }
    }

    val src = driver.watCode
    checkOut("print_float", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(2.4000000953674316.toFloat)
    })
  }

  test("print_double") {
    val driver = new DslDriverWasm[Double,Unit] {
      @virtualize
      def snippet(arg: Rep[Double]) = {
        println(arg)
      }
    }

    val src = driver.watCode
    checkOut("print_double", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(2.4000000953674316.toDouble)
    })
  }

  test("print_boolean") {
    val driver = new DslDriverWasm[Boolean,Unit] {
      @virtualize
      def snippet(arg: Rep[Boolean]) = {
        println(arg)
      }
    }

    val src = driver.watCode
    checkSnippet("print_boolean", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval(true)
      driver.eval(false)
    })
  }

  test("print_string") {
    val driver = new DslDriverWasm[String,Unit] {
      @virtualize
      def snippet(arg: Rep[String]) = {
        println(arg)
      }
    }

    val src = driver.watCode
    checkSnippet("print_string", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval("Hello World!")
    })
  }
}

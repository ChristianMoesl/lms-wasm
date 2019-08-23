package lms.wasm

import lms.TutorialFunSuite
import lms.core.virtualize
import lms.macros.SourceContext

class StringsTest extends TutorialFunSuite {
    val under = "backend/strings/"

  test("string_slice") {
      val driver = new DslDriverWasm[String,Unit] {
          @virtualize
          def snippet(arg: Rep[String]) = {
              println(arg.substring(3, 6))
          }
      }

      val src = driver.watCode
      checkOut("string_slice", driver.jsCode, {
              println(src)
              println(";; output:")
              driver.eval("Hello World!")
      })
  }

  test("string_toInt") {
    val driver = new DslDriverWasm[String,Unit] {
      @virtualize
      def snippet(arg: Rep[String]) = {
        println(arg.toInt)
      }
    }

    val src = driver.watCode
    checkOut("string_toInt", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval("135")
    })
  }

  test("string_toDouble") {
    val driver = new DslDriverWasm[String,Unit] {
      @virtualize
      def snippet(arg: Rep[String]) = {
        println(arg.toDouble)
      }
    }

    val src = driver.watCode
    checkOut("string_toDouble", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval("135.15499877929688")
    })
  }

  test("string_length") {
    val driver = new DslDriverWasm[String,Unit] {
      @virtualize
      def snippet(arg: Rep[String]) = {
        println(arg.length)
      }
    }

    val src = driver.watCode
    checkOut("string_length", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval("hello world!")
    })
  }

  test("string_charAt") {
    val driver = new DslDriverWasm[String,Unit] {
      @virtualize
      def snippet(arg: Rep[String]) = {
        println(arg.charAt(3))
      }
    }

    val src = driver.watCode
    checkOut("string_charAt", driver.jsCode, {
      println(src)
      println(";; output:")
      driver.eval("Hello World!")
    })
  }
}

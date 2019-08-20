package lms.wasm

import lms.TutorialFunSuite
import lms.core.virtualize
import lms.macros.SourceContext

class VariablesTest extends TutorialFunSuite {
  val under = "backend/"

  test("variable_r") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        val y: Rep[Int] = x
        val z: Rep[Int] = x
        printf("%d\n", y + z)
      }
    }

    val src = driver.code
    checkOut("variable_r", {
      println(src)
      println(";; output:")
      driver.eval(4)
    })
  }

  test("variable_w_dead") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        x = 3
        printf("A\n")
      }
    }
    val src = driver.code
    checkOut("variable_w_dead", {
      println(src)
      println(";; output:")
      driver.eval(4)
    })
  }

  test("variable_w") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        x = 3
        x = 4
        printf("%d\n", x)
      }
    }
    val src = driver.code
    checkOut("variable_w", {
      println(src)
      println(";; output:")
      driver.eval(4)
    })
  }

  test("variable_rw") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        val y: Rep[Int] = x
        x = 3
        val z: Rep[Int] = x
        printf("%d\n", y + z)
      }
    }
    val src = driver.code
    checkOut("variable_rw", {
      println(src)
      println(";; output:")
      driver.eval(5)
    })
  }

  test("variable_while_dead1") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          x += 1
        }
      }
    }
    val src = driver.code
    checkOut("variable_while_dead1", {
      println(src)
      println(";; output:")
      driver.eval(5)
    })
  }

  test("variable_while_dead2") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        val a = NewArray[Int](10)
        while (x < 10) {
          a(x) = x
          x += 1
        }
      }
    }
    val src = driver.code
    checkOut("variable_while_dead2", {
      println(src)
      println(";; output:")
      driver.eval(5)
    })
  }

  test("variable_while1") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          x += 1
        }
        printf("%d\n", x)
      }
    }
    val src = driver.code
    checkOut("variable_while1", {
      println(src)
      println(";; output:")
      driver.eval(5)
    })
  }

  test("variable_while2") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          printf("%d\n", x)
          x += 1
        }
      }
    }
    val src = driver.code
    checkOut("variable_while2", {
      println(src)
      println(";; output:")
      driver.eval(5)
    })
  }

  test("variable_while3") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        while (x < 10) {
          printf("A\n")
          x += 1
        }
      }
    }
    val src = driver.code
    checkOut("variable_while3", {
      println(src)
      println(";; output:")
      driver.eval(5)
    })
  }

  test("variable_while4") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        var y = 0
        var z = 0
        var t = 6
        while (x < 10) {
          z += y
          y += 1
          t += z
          x += 1
        }
        printf("%d\n", z)
      }
    }
    val src = driver.code
    checkOut("variable_while4", {
      println(src)
      println(";; output:")
      driver.eval(5)
    })
  }

  test("variable_if_nested") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        var y = 0
        if (arg > 10) {
          if (arg < 20) {
            x += 1
            y += 4
          }
        }
        printf("%d\n", x)
      }
    }
    val src = driver.code
    checkOut("variable_if_nested", {
      println(src)
      println(";; output:")
      driver.eval(10)
      driver.eval(11)
      driver.eval(19)
      driver.eval(20)
    })
  }

  test("variable_if_nested_dead") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        if (arg > 10) {
          if (arg < 20) x += 1
        }
      }
    }
    val src = driver.code
    checkOut("variable_if_nested_dead", {
      println(src)
      println(";; output:")
      driver.eval(10)
      driver.eval(11)
      driver.eval(19)
      driver.eval(20)
    })
  }

  test("variable_write_after_read") {
    val driver = new DslDriverWasm[Int, Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        printf("%d\n", x)
        if (arg > 0)
          x += 1
        else
          x += 2
        printf("%d\n", x)
      }
    }

    val src = driver.code
    checkOut("variable_write_after_read", {
      println(src)
      println(";; output:")
      driver.eval(-1)
      driver.eval(1)
    })
  }

  test("variable_while_deps_read") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        var x = 1
        val y: Rep[Int] = x
        if (arg < 50) {
          while (x < 10) {
            printf("%d\n", x)
            x += 1
          }
          printf("%d\n", y)
        }
        val z: Rep[Int] = x
        x += 1
        printf("%d\n", z)
        printf("%d\n", x)
      }
    }
    val src = driver.code
    checkOut("variable_while_deps_read", {
      println(src)
      println(";; output:")
      driver.eval(2)
      driver.eval(50)
    })
  }
}

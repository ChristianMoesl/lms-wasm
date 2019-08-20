package lms.wasm

import lms.TutorialFunSuite
import lms.core.stub.DslDriverC
import lms.core.virtualize
import lms.macros.SourceContext

class CommentTest extends TutorialFunSuite {
  val under = "backend/comment/"

  test("generate-comment") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = arg + 1
        generate_comment("This is a very important comment")
        printf("%d\n", x)
      }
    }

    val src = driver.code
    checkSnippet("generate-comment", {
      println(src)
      println(";; output:")
      driver.eval(1)
    })
  }

  test("comment") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        comment("This is an important block comment") {
          val x = arg + 1
          printf("%d\n", x)
        }
      }
    }

    val src = driver.code
    checkSnippet("comment", {
      println(src)
      println(";; output:")
      driver.eval(1)
    })
  }

  test("comment_with_result") {
    val driver = new DslDriverWasm[Int,Unit] {
      @virtualize
      def snippet(arg: Rep[Int]) = {
        val x = comment("This is an important block comment") {
          arg + 1
        }
        printf("%d\n", x)
      }
    }

    val src = driver.code
    checkSnippet("comment_with_result", {
      println(src)
      println(";; output:")
      driver.eval(1)
    })
  }
}

package lms.wasm

import lms.core.stub._
import lms.core.utils.time

trait DslGenWasm extends WasmGenBase {
  val IR: DslExp
}

abstract class DslDriverWasm[A: Manifest, B: Manifest] extends DslSnippet[A, B] with DslExp {
  q =>
  val codegen = new DslGenWasm {
    val IR: q.type = q
  }
  lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = codegen.emitSource[A,B](wrapper, "Snippet", new java.io.PrintStream(source))
    (source.toString, statics)
  }

  lazy val f: A => Unit = {
    val out = new java.io.PrintStream("/tmp/snippet.wat")
    out.println(code)
    out.close
    import scala.sys.process._
    (a: A) => {
      // Express negativ integers in 2's complement because wasmtime can not handle negative integers as argument
      val arg = a match {
        case i: Int if i < 0 => s"""$$((2 ** 32 - ${i * (-1)}))"""
        case _ => a
      }

      (s"wasmtime /tmp/snippet.wat $arg": ProcessBuilder).lines.foreach(Console.println _)
    }
  }
  def eval(a: A): Unit = { val f1 = f; time("eval")(f1(a)) }
}

trait WasmGenBase extends ExtendedWasmCodeGen {
  val IR: Base
  import IR._
  // def remap[A](m: Manifest[A]): String = ???
  // def quote(x: Exp[Any]) : String = ???
  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = ???
  def emitSource[A : Manifest, B : Manifest](f: Rep[A]=>Rep[B], className: String, stream: java.io.PrintStream): List[(Class[_], Any)] = {
    val statics = Adapter.emitCommon1(className,this,stream)(manifest[A],manifest[B])(x => Unwrap(f(Wrap[A](x))))
    // stream.println(src)
    statics.toList
  }
  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: java.io.PrintStream): List[(Sym[Any], Any)] = ???
}
package lms.wasm

import java.io.PrintStream

import lms.core.stub._
import lms.core.utils.{time,withTiming,nullout}

trait DslGenWasm extends WasmGenBase with WasmGenNumericOps
  with WasmGenPrimitiveOps with WasmGenBooleanOps with WasmGenIfThenElse
  with WasmGenEqual with WasmGenOrderingOps
  with WasmGenMiscOps with WasmGenArrayOps with WasmGenStringOps
  with WasmGenFunctions with WasmGenWhile
  with WasmGenStaticData with WasmGenVariables
  with WasmGenUtilOps {
  val IR: DslExp
}

abstract class DslDriverWasm[A: Manifest, B: Manifest] extends DslSnippet[A, B] with DslExp {
  q =>
  val codegen = new DslGenWasm {
    val IR: q.type = q
  }
  lazy val (jsCode, watCode, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = codegen.emitSource[A,B](wrapper, "Snippet", new java.io.PrintStream(source))

    val splitted = source.toString.split("---\n")

    (splitted(1), splitted(0), statics)
  }

  lazy val f: A => Unit = {
    val jsOut = new PrintStream("/tmp/snippet.js")
    jsOut.println(jsCode)
    jsOut.close

    val watOut = new PrintStream("/tmp/snippet.wat")
    watOut.println(watCode)
    watOut.close

    import scala.sys.process._

    (a: A) => {
      Seq("wat2wasm", "/tmp/snippet.wat", "-o", "/tmp/snippet.wasm").lineStream.foreach(Console.println)
      Seq("node", "--max-old-space-size=12288", "/tmp/snippet.js", s"$a").lineStream.foreach(Console.println)
    }
  }
  def eval(a: A): Unit = { val f1 = f; time("eval")(f1(a)) }
}

trait WasmGenBase extends ExtendedWasmCodeGen {
  val IR: Base
  import IR._
  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = ???
  def emitSource[A : Manifest, B : Manifest](f: Rep[A]=>Rep[B], className: String, stream: java.io.PrintStream): List[(Class[_], Any)] = {
    withTiming[List[(Class[_], Any)]](nullout, nullout) {
      val statics = Adapter.emitCommon1(className,this, stream)(manifest[A],manifest[B])(x => Unwrap(f(Wrap[A](x))))
      statics.toList
    }
  }
  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: java.io.PrintStream): List[(Sym[Any], Any)] = ???
}

trait WasmGenNumericOps extends WasmGenBase
trait WasmGenPrimitiveOps extends WasmGenBase
trait WasmGenBooleanOps extends WasmGenBase
trait WasmGenIfThenElse extends WasmGenBase
trait WasmGenEqual extends WasmGenBase
trait WasmGenOrderingOps extends WasmGenBase
trait WasmGenMiscOps extends WasmGenBase
trait WasmGenArrayOps extends WasmGenBase
trait WasmGenStringOps extends WasmGenBase
trait WasmGenFunctions extends WasmGenBase
trait WasmGenWhile extends WasmGenBase
trait WasmGenStaticData extends WasmGenBase
trait WasmGenVariables extends WasmGenBase
trait WasmGenUtilOps extends WasmGenBase
trait WasmGenUncheckedOps extends WasmGenBase
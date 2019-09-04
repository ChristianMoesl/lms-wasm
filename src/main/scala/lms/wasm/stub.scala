package lms.wasm

import java.io.PrintStream

import lms.core.stub._
import lms.core.utils.time

trait DslGenWasm extends WasmGenBase with CGenNumericOps
  with CGenPrimitiveOps with CGenBooleanOps with CGenIfThenElse
  with CGenEqual with CGenRangeOps with CGenOrderingOps
  with CGenMiscOps with CGenArrayOps with CGenStringOps
  with CGenSeqOps with CGenFunctions with CGenWhile
  with CGenStaticData with CGenVariables
  with CGenObjectOps
  with CGenUtilOps {
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
    val statics = Adapter.emitCommon1(className,this,stream)(manifest[A],manifest[B])(x => Unwrap(f(Wrap[A](x))))
    // stream.println(src)
    statics.toList
  }
  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: java.io.PrintStream): List[(Sym[Any], Any)] = ???
}
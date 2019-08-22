package lms.wasm

import scala.collection.mutable
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.{Files, Path}

import lms.core._
import lms.core.Backend._
import lms.wasm.WasmType.WasmType

class WasmSection(val id: Int) {
  private val nameMap = Array("custom", "type", "import", "function", "table", "memory", "global", "export", "start", "element", "code", "data")
  private val registered = mutable.HashSet[String]()
  private val stream = new ByteArrayOutputStream()
  private val writer = new PrintStream(stream)
  private var ongoingData = false

  def register(id: String, gen: ExtendedWasmCodeGen)(f: => Unit) = if (!registered(id)) {
    val save = gen.stream
    gen.stream = writer

    if (ongoingData) ???
    ongoingData = true
    registered += id
    f
    ongoingData = false

    gen.stream = save
  }

  def emit(to: PrintStream) = {
    if (stream.size > 0) {
      to.println(s"\n;; *********** ${nameMap(id).capitalize} Section ***********")
      stream.writeTo(to)
    }
  }
}

object WasmType extends Enumeration {
  type WasmType = Value
  val int32 = Value("i32")
  val int64 = Value("i64")
  val float32 = Value("f32")
  val float64 = Value("f64")
}

case class TypeFeature(wasmType: WasmType, signed: Boolean = true, bits: Int = 32, ptr: Boolean = false)

class SymbolEntry(name: String)
case class Variable(id: String, m: Manifest[_]) extends SymbolEntry(id)
case class Function(id: String, paramTypes: List[Manifest[_]], resultType: Option[Manifest[_]]) extends SymbolEntry(id)
case class StringLiteral(id: String, offset: Int) extends SymbolEntry(id)
case class DataBlock(id: String, offset: Int, size: Int) extends SymbolEntry(id)

class ExtendedWasmCodeGen extends CompactTraverser with ExtendedCodeGen {

  private val maxIntStringLength = (math.ceil(math.log10(math.pow(2, 64))) + 2).toInt

  var lastNL = false
  def emit(s: String): Unit = { stream.print(s); lastNL = false }
  def emitln(s: String = "") = if (s != "" || !lastNL) { stream.println(s); lastNL = true }

  def array(innerType: String): String = ???
  def function(sig: List[Manifest[_]]): String = ???
  def nameMap: Map[String,String] = ???


  def record(man: lms.macros.RefinedManifest[_]): String = ???
  def remapUnsigned(m: Manifest[_]): String = ???

  val rename = new mutable.HashMap[Sym,String]

  def quote(s: Def): String = s match {
    case s @ Sym(n) => rename.getOrElseUpdate(s, s"x${rename.size}")
    case Const(s: String) => "\""+s.replace("\"", "\\\"")
      .replace("\n","\\n")
      .replace("\t","\\t")+"\\00\"" // TODO: more escapes?
    case Const(b: Boolean) => if (b) "1" else "0"
    case Const(x) => x.toString
  }

  override def typeBlockRes(x: Exp): Manifest[_] = x match {
    case Const(x: Boolean) => manifest[Boolean]
    case _ => super.typeBlockRes(x)
  }

  // Remap auxiliary function C specific
  override def primitive(rawType: String): String = rawType match {
    case "Unit" => ""
    case "Boolean" => "i32"
    case "Int" => "i32"
    case "Long" => "i64"
    case "Float" => "f32" // TODO: Check if this is a translation
    case "Double" => "f64"
    case "java.lang.String" => "i32"  // pointer
    case _ => ???
  }

  def typeFeature(m: Manifest[_]): TypeFeature = m.toString match {
    case "Boolean" => TypeFeature(WasmType.int32, true, 32, false)
    case "Int" => TypeFeature(WasmType.int32, true, 32, false)
    case "Long" => TypeFeature(WasmType.int64, true, 64, false)
    case "Float" => TypeFeature(WasmType.float32, true, 32, false) // TODO: Check if this is a valid translation
    case "Double" => TypeFeature(WasmType.float64, true, 64, false)
    case "java.lang.String" => TypeFeature(WasmType.int32, true, 8, true) // pointer
    case _ =>  ???
  }


  def emitFunctionCall(id: String)(f: => Unit) {
    f
    emitln(s"call $$$id")

    val fun = symbolTable(id).asInstanceOf[Function]

    stack.remove(0, fun.paramTypes.length)

    if (fun.resultType.isDefined)
      push(fun.resultType.get)

 //   emitln("drop") // TODO: Where to drop result?
  }

  val unaryop = Set("-","!","&")

  def remapUnaryOp(id: String) = id match {
    case "!" => "eqz"
    case "-" => ???
    case "&" => ???
  }

  def emitUnaryOperation(id: String): Unit = {
    val t = pop()

    emit(s"${remap(t)}.${remapUnaryOp(id)}")

    push(manifest[Boolean])
  }

  val binop = Set("+","-","*","/","%","==","!=","<",">",">=","<=","&","|","<<",">>", ">>>", "&&", "||", "^")

  def sign(signed: Boolean) = if (signed) "s" else "u"

  def remapBinOp(id: String, s: Boolean) = id match {
    case "+" => "add"
    case "-" => "sub"
    case "*" => "mul"
    case "/" => s"div_${sign(s)}"
    case "==" => "eq"
    case "!=" => "ne"
    case "<" => s"lt_${sign(s)}"
    case ">" => s"gt_${sign(s)}"
    case "<=" => s"le_${sign(s)}"
    case ">=" => s"ge_${sign(s)}"
    case "&" => "and"
    case "|" => "or"
    case "<<" => "shl"
    case ">>" => s"shr_${sign(s)}"
    case ">>>" => s"shr_${sign(s)}"
    case "%" => s"rem_${sign(s)}"
    case "&&" => ???
    case "||" => ???
    case "^" => "xor"
  }

  val comparisonop = Set("==", "!=", "<", ">", ">=", "<=")

  def emitBinaryOperation(id: String): Unit = {
    val rhs = pop()
    val lhs = pop()
    val f = typeFeature(rhs)

    assert(lhs == rhs, s"Type convertion is mandatory: $lhs != $rhs")

    emitln(s"${remap(rhs)}.${remapBinOp(id, f.signed)}")

    if (comparisonop contains id)
      push(manifest[Boolean])
    else
      push(rhs)
  }

  def emitStrLen(): Unit = {
    if (symbolTable contains "strlen") return

    registerFunction("strlen", List(manifest[String]), Some(manifest[Int]),
      """
        |(func $strlen (param $str i32) (result i32)
        |    (if (i32.eqz (i32.load8_u (local.get $str)))
        |    (then
        |      (return (i32.const 0))
        |    ))
        |    (return
        |      (i32.add
        |        (call $strlen (i32.add (local.get $str) (i32.const 1)))
        |        (i32.const 1)
        |      ))
        |  )
        |""".stripMargin)
  }

  // TODO: grow memory with "memory.grow" (similar to brk())
  def emitMalloc(): Unit = {
    if (symbolTable contains "malloc") return

    registerGlobalVariable("_bump", manifest[Int], 1024) // TODO: verify value of hardcoded bump pointer
    registerFunction("malloc", List(manifest[Int]), Some(manifest[Int]),
      """
        |(func $malloc (param $size i32) (result i32)
        |  (global.get $_bump)
        |  (i32.add (local.get $size) (global.get $_bump))
        |  (global.set $_bump)
        |)
        |""".stripMargin)
  }

  def emitAtoi(): Unit = {
    if (symbolTable contains "atoi") return

    registerFunction("atoi", List(manifest[String]), Some(manifest[Int]),
      """
        |(func $_atoi_rec (param $sum i32) (param $str i32) (result i32)
        |    (if (result i32) (i32.eqz (i32.load8_u (local.get $str)))
        |      (then (local.get $sum))
        |      (else
        |        (call $_atoi_rec
        |          (i32.add (i32.mul (local.get $sum) (i32.const 10))
        |                  (i32.sub (i32.load8_u (local.get $str)) (i32.const 0x30)))
        |          (i32.add (local.get $str) (i32.const 1)))
        |  ))
        |)
        |(func $atoi (param $str i32) (result i32)
        |  (call $_atoi_rec (i32.const 0) (local.get $str))
        |)
        |""".stripMargin)
  }

  def emitItoa(): Unit = {
    if (symbolTable contains "itoa") return

    emitMalloc()

    registerFunction("itoa", List(manifest[Int], manifest[String], manifest[Int]), Some(manifest[Int]),
      """
        |(func $_itoa_rec (param $i i32) (param $str i32) (param $base i32) (result i32)
        |    (local $idx i32)
        |    (if (i32.eqz (local.get $i))
        |    (then
        |      (return (i32.const 0)))
        |    )
        |    (call $_itoa_rec (i32.div_u (local.get $i) (local.get $base)) (local.get $str) (local.get $base))
        |    (local.set $idx)
        |    (i32.store8
        |      (i32.add (local.get $str) (local.get $idx))
        |      (i32.add (i32.rem_u (local.get $i) (local.get $base)) (i32.const 48))
        |    )
        |    (i32.add (local.get $idx) (i32.const 1))
        |  )
        |  (func $itoa (param $i i32) (param $str i32) (param $base i32) (result i32)
        |    (if (result i32) (i32.eqz (local.get $i))
        |    (then
        |      (i32.store8 (local.get $str) (i32.const 48))
        |      (i32.const 1)
        |    )
        |    (else
        |      (call $_itoa_rec (local.get $i) (local.get $str) (local.get $base))
        |    ))
        |  )
        |""".stripMargin)
  }

  def emitEnvironmentImports(): Unit = {
    registerDataBlock("syscall_params", 4 * 5)
    registerEnvironmentFunction("printlnString", List(manifest[String]))
    registerEnvironmentFunction("printlnInt", List(manifest[Int]))
    registerEnvironmentFunction("printlnFloat", List(manifest[Float]))
    registerEnvironmentFunction("printlnBoolean", List(manifest[Boolean]))
    registerEnvironmentFunction("exit", List(manifest[Int]))
  }

  private val importSection = new WasmSection(id = 2)
  private val memorySection = new WasmSection(id = 5)
  private val globalSection = new WasmSection(id = 6)
  private val exportSection = new WasmSection(id = 7)
  private val codeSection = new WasmSection(id = 10)
  private val dataSection = new WasmSection(id = 11)
  private var dataOffset = 0

  private var symbolTable = new mutable.HashMap[String, SymbolEntry]

  private var stack = mutable.ListBuffer[Manifest[_]]()

  private def push(m: Manifest[_]): Unit = {
    stack += m
//    emitln(s";; size = ${stack.size}")
  }

  private def pop() = {
//    emitln(s";; size = ${stack.size - 1}")
    stack.remove(0)
  }

  def registerGlobalVariable(id: String, m: Manifest[_], value: Int = 0): Unit = {
    require(m != manifest[Unknown])
    symbolTable += (id -> Variable(id, m))
    globalSection.register(id, this) {
      emitln(s"""(global $$$id (mut ${remap(m)}) (${remap(m)}.const $value))""")
    }
  }

  def registerDataBlock(id: String, size: Int): Unit = {
    symbolTable += (id -> DataBlock(id, dataOffset, size))
    dataSection.register(id, this) {
      emitln(s"""(data (i32.const ${dataOffset}) "${"\\00".repeat(size)}")""")
    }
    dataOffset += size
  }

  def registerString(string: Const): Int = registerString(quote(string))
  def registerString(string: String): Int = {
    symbolTable += (string -> StringLiteral(string, dataOffset))
    dataSection.register(string, this) {
      emitln(s"""(data (i32.const ${dataOffset}) $string)""")
    }
    val result = dataOffset
    dataOffset += string.length
    result
  }

  def registerEnvironmentFunction(id: String, paramTypes: List[Manifest[_]], resultType: Option[Manifest[_]] = None) = {
    require(paramTypes.nonEmpty)

    symbolTable += (id -> Function(id, paramTypes, resultType))

    importSection.register(id, this) {
      val pTypes = paramTypes.map(t => remap(t)).mkString(" ")

      val result = if (resultType.isDefined) s" (result ${remap(resultType.get)})" else ""

      emitln(s"""(import "env" "$id" (func $$$id (param $pTypes)$result))""")
    }
  }

  def registerFunction(id: String, paramTypes: List[Manifest[_]], resultType: Option[Manifest[_]] = None, code: String = "") = {
    symbolTable += (id -> Function(id, paramTypes, resultType))

    if (code != "") {
      codeSection.register(id, this) {
        emitln(code)
      }
    }
  }

  private val localVariableStream = new ByteArrayOutputStream()
  private val localVariableWriter = new PrintStream(localVariableStream)

  def emitLocalVariable(id: String, m: Manifest[_]): Unit = {
    withStream(localVariableWriter) {
      emitln(s"(local $$$id ${remap(m)})")
    }
  }

  def emitFunction(name: String, body: Block): Unit = {
    val res = body.res
    val args = body.in

    emit(s"""(func $$$name (export "$name") ${args map(s => s"(param $$${quote(s)} ${remap(t(s))})") mkString(" ")}""")
    emitResultType(t(res)); emitln()

    registerFunction(name, args.map(a => t(a)), if (res != Const(())) Some(t(res)) else None)

    localVariableStream.write(capture(traverse(body)).toByteArray)
    stream.write(localVariableStream.toByteArray)

    emitln(")")

    if (res != Const(()))
      pop()

  } ensuring(stack.isEmpty, s"stack has ${stack.size} elements at end of function $name")

  def scope(variable: Def): String = {
    if ((symbolTable contains quote(variable)) && (symbolTable(quote(variable)).isInstanceOf[Variable]))
      "global"
    else
      "local"
  }

  // process and print block results
  override def traverseCompact(ns: Seq[Node], y: Block): Unit = {
    wraper(numStms, lastNode, y) {
      super.traverseCompact(ns, y)
    }
  }

  private def emitVarSet(sym: Sym) = { emitln(s"${scope(sym)}.set $$${quote(sym)}"); pop() }

  private def emitResultType(m: Manifest[_]) = emit(if (m == manifest[Unit]) "" else s" (result ${remap(m)})")

  private def emitConvert(to: Manifest[_]): Unit = {
    val from = pop()

    require(from != to)

    emit(s"${remap(to)}.")

    def wrapChar(): Unit = {
      emitln(s"i32.const 255")
      emitln("and")
    }

    if (from == manifest[Boolean]) {
      if (to == manifest[Int] || to == manifest[Char]) ""
      else if (to == manifest[Long]) emit(s"extend_u/${remap(from)}")
      else if (to == manifest[Float] || to == manifest[Double]) emit(s"convert_u/${remap(from)}")
      else ???
    } else if (from == manifest[Int] || from == manifest[Long]) {
      if (to == manifest[Boolean]) ??? // TODO: do we need this case?
      else if (to == manifest[Char]) { emitln(s"wrap_s/${remap(from)}"); wrapChar() }
      else if (from == manifest[Int] && to == manifest[Long]) emit(s"extend_s/${remap(from)}")
      else if (from == manifest[Long] && to == manifest[Int]) emit(s"wrap_s/${remap(from)}")
      else if (to == manifest[Float] || to == manifest[Double]) emit(s"convert_s/${remap(from)}")
      else ???
    } else if (from == manifest[Float] || from == manifest[Double]) {
      if (to == manifest[Boolean]) ??? // TODO: do we need this case?
      else if (to == manifest[Char]) { emitln(s"trunc_u/${remap(from)}"); wrapChar() }
      else if (from == manifest[Float] && to == manifest[Double]) emit(s"promote/${remap(from)}")
      else if (from == manifest[Double] && to == manifest[Float]) emit(s"demote/${remap(from)}")
      else if (to == manifest[Int] || to == manifest[Long]) emit(s"trunc_s/${remap(from)}")
      else ???
    } else ???

    emitln()

    push(to)
  }

  private def emitIf(result: Manifest[_] = manifest[Unknown])(f: () => Unit) = emitIfThenElse(result)(f, None)
  private def emitIfThenElse(result: Manifest[_] = manifest[Unknown])(thenF: () => Unit, elseF: Option[() => Unit]): Unit = {
    emit("if"); emitResultType(result); emitln(); pop()
    thenF()
    elseF.foreach { f =>
      val ifType = pop()
      emitln("else")
      f()
      assert(ifType == stack.head)
    }
    emitln("end")
  }

  private def emitBr(label: String) = emitln(s"br $label")
  private def emitBrIf(label: String) = { emitln(s"br_if $label"); pop() }

  private def emitLoop(f: (String) => Unit) = emitBlockKind("loop")(f)
  private def emitBlock(f: (String) => Unit) = emitBlockKind("block")(f)
  private def emitBlockKind(kind: String)(f: (String) => Unit): Unit = {
    val label = s"$$${allocateControlLabel()}"
    emitln(s"$kind $label")
    f(label)
    emitln("end")
  }

  var controlLabel = 0
  override def traverse(n: Node): Unit = n match {

    // Variables
    case n @ Node(s, "var_new", List(exp), eff) =>
      emitLocalVariable(quote(s), t(s))
      shallow(exp); emitln()
      emitVarSet(s)

    case n @ Node(_, "var_set", List(x: Sym, y), _) =>
      shallow(y); emitln()
      emitVarSet(x)

    // Control flow
    case n @ Node(_,"?",c::(a:Block)::(b:Block)::_,_) =>
      shallow(c); emitln()
      emitln(s"if "); pop()
      quoteBlock(traverse(a))
      quoteElseBlock(traverse(b))

    case n @ Node(_,"W",List(c:Block,b:Block),_) =>
      emitBlock { block => emitLoop { loop =>
        quoteBlock(traverse(c))
        emitln(s"${remap(stack.head)}.eqz")
        emitBrIf(block)
        traverse(b)
        emitBr(loop)
      }}

    case n @ Node(_, "switch", (guard: Exp)::default::others, _) =>
      val switchTypes = Set[Manifest[_]](manifest[Long], manifest[Int], manifest[Short], manifest[Char])

      assert(switchTypes contains t(guard))

      emitBlock { breakLabel =>
        others.grouped(2).foreach {
          case Seq(Const(cases), block: Block) =>
            emitBlock { nextCaseLabel =>
              emitBlock { caseLabel =>
                cases.asInstanceOf[Seq[Const]].foreach { x =>
                  shallow(guard)
                  shallow(x)
                  emitBinaryOperation("==")
                  emitBrIf(caseLabel)
                }
                emitBr(nextCaseLabel)
              }
              quoteBlock(traverse(block))

              emitBr(breakLabel)
            }
        }
        default match {
          case block: Block => quoteBlock(traverse(block))
          case _ =>
        }
      }

    // Comments
    case n @ Node(_,"generate-comment", List(Const(x: String)),_) =>
      emit(";; "); emitln(x)

    case n @ Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
      emitln(";;# " + str)
      if (verbose) {
        emitln(";; generated code for " + str.replace('_', ' '))
      } else {
        emitln(";; generated code")
      }
      if (dce.live(s)) {
        emitLocalVariable(quote(s), t(s))
        quoteBlock(traverse(b))
        emitVarSet(s)
      } else
        traverse(b)

      emitln("\n;;# " + str)

    // MiscOps
    case n @ Node(_, "printf", List(Const("%d\n"), exp), _) =>
      emitFunctionCall("printlnInt") {
        shallow(exp)
      }

    case n @ Node(_, "printf", List(string), _) =>
      emitFunctionCall("printlnString") {
        shallow(string)
      }

    case n @ Node(_, "P", List(x), _) =>
      shallow(x); emitln()

      if (stack.head == manifest[Int] || stack.head == manifest[Long]) {
        if (stack.head == manifest[Long])
          emitConvert(manifest[Int])

        emitFunctionCall("printlnInt") { }
      } else if (stack.head == manifest[Float] || stack.head == manifest[Double]) {
        if (stack.head == manifest[Double])
          emitConvert(manifest[Float])

        emitFunctionCall("printlnFloat") { }
      } else if (stack.head == manifest[Boolean])
        emitFunctionCall("printlnBoolean") { }
      else if (stack.head == manifest[Char]) {
        ???
      } else if (stack.head == manifest[String])
        emitFunctionCall("printlnString") { }

    case n @ Node(_, _, List(_, _), _) => // val defintion
      emitLocalVariable(quote(n.n), t(n.n))
      shallow(n); emitln()
      emitVarSet(n.n)

    case _ =>
      println("============traverse(n: Node)")
      println(n)
      println("============")
  }

  def nowraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = f
  var wraper: WrapFun = nowraper _

  type WrapFun = (Int, Option[Node], Block) => (=> Unit) =>Unit
  def withWraper(w: WrapFun)(f: => Unit) = {
    val save = wraper
    wraper = w
    f
    wraper = save // needed?
  }

  def quoteBlock(f: => Unit) = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      f
      if (y.res != Const(())) shallow(y.res)
    }
    withWraper(wraper _)(f)
  }

  def quoteElseBlock(f: => Unit) = {
    def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
      if (numStms > 0) {
        emitln("else")
        f
        if (y.res != Const(())) shallow(y.res)
      }
      emitln("end")
    }
    withWraper(wraper _)(f)
  }

  def allocateControlLabel(): Int = {
    val tmp = controlLabel
    controlLabel += 1
    tmp
  }

  def t(e: lms.core.Backend.Exp): Manifest[_] = typeBlockRes(e)

  def shallow(n: Node): Unit = n match {

    case n @ Node(s, "!", List(exp), _) =>
      shallow(exp); emitln()
      emitUnaryOperation("!"); emitln()
      assert(t(s) == stack.head, s"${t(s)} != ${stack.head}")

    case n @ Node(s, op, List(lhs, rhs), _) =>
      shallow(lhs); emitln()
      shallow(rhs); emitln()
      emitBinaryOperation(op)
      assert(t(s) == stack.head, s"${t(s)} != ${stack.head}")

    case n @ Node(s,"?",List(c, a: Block, b: Block),_) if a.isPure && a.res == Const(true) =>
      shallow(c)
      emitIfThenElse(manifest[Boolean])(() => shallow(a.res), Some(() => quoteBlock(traverse(b))))

    case n @ Node(s,"?",List(c, a: Block, b: Block),_) if b.isPure && b.res == Const(false) =>
      shallow(c)
      emitIfThenElse(manifest[Boolean])(() => quoteBlock(traverse(a)), Some(() => shallow(b.res)))

    case n @ Node(s,"var_get", List(x), _) =>
      if (dce.live(s))
        shallow(x)

    case _ =>
      println("============shallow(n: Node)")
      println(n)
      println("============")
  }

  def shallow(n: Def): Unit = n match {
    case InlineSym(n) => shallow(n)

    case n @ Sym(_) =>
      emitln(s"${scope(n)}.get $$${quote(n)}"); push(t(n))

    case n @ Const(s: String) =>
      val offset = registerString(n)
      emitln(s"${remap(manifest[String])}.const $offset"); push(manifest[String])

    case n @ Const(_) =>
      emitln(s"${remap(t(n))}.const ${quote(n)}"); push(t(n))

    case _ =>
      println("============shallow(n: Def)")
      println(n)
      println("============")
  }

  def run(name: String, g: Graph) = {
    capture {
      bound(g)
      withScope(Nil, g.nodes) {
        emitFunction(name, g.block)
      }
    }
  }

  def emitLibrary(call: String): Unit = {
    emitEnvironmentImports()
    emitMalloc()

    memorySection.register("1", this) { emitln("(memory (export \"mem\") 1)") }
  }

  def generateJavaScript(name: String)(m1:Manifest[_],m2:Manifest[_]): String = {
    val paramTypes = symbolTable(name).asInstanceOf[Function].paramTypes

    def args(sep: String) = paramTypes.indices map (n => s"arg$n") mkString sep

    def toJsType(m: Manifest[_]) = if (m == manifest[Int] || m == manifest[Boolean]) "Int" else "Float"

    def parseArg(i: Int, m: Manifest[_]) =
      if (m1 == manifest[Boolean])
        s"""const arg$i = (process.argv[${i + 2}] === "true") ? true :
             (process.argv[${i + 2}] === "false") ? false : Number.NaN;"""
      else if (m1 == manifest[String])
        ""
      else
        s"const arg$i = Number.parse${toJsType(m)}(process.argv[${i + 2}]);"

    val parseArgs = paramTypes.zipWithIndex.map(a => parseArg(a._2, a._1)).mkString("\n")

    val checkArgs = if (m1 == manifest[String]) "if (false)" else s"if (${paramTypes.indices map (i => s"isNaN(arg$i)") mkString " || "})"

    val insertString = if (m1 != manifest[String]) "" else
      s"""const arg0 = $dataOffset;""" + "\n" + paramTypes.indices
        .map(i => s"""const arg${i + 1} = insertAt(new Uint8Array(mem.buffer), arg$i, process.argv[${i + 2}]);""")
        .mkString("\n")

    val printUsage = s"""console.error(`usage: node $${process.argv[1]} ${args(" ")}`);"""

    Files.readString(Path.of("src/main/js/environment.js"))
      .replace("/*PARSE_ARGS*/", parseArgs)
      .replace("/*CHECK_ARGS*/", checkArgs)
      .replace("/*PRINT_USAGE*/", printUsage)
      .replace("/*INSERT_STRINGS*/", insertString)
      .replace("/*FUNCTION_CALL*/",s""".$name(${args(", ")})""")
  }

  override def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit = {
    require(m1 == manifest[Int] || m1 == manifest[Float] || m1 == manifest[Boolean] || m1 == manifest[String])
    require(m2 == manifest[Unit])

    val ng = init(g)

    emitln("(module")

    emitLibrary(name)

    val src = run(name, ng)
    codeSection.register("staged_code", this) {
      emit(src)
    }

    importSection.emit(to = stream)
    memorySection.emit(to = stream)
    globalSection.emit(to = stream)
    exportSection.emit(to = stream)
    codeSection.emit(to = stream)
    dataSection.emit(to = stream)

    emitln(")")

    emitln("---")

    emit(generateJavaScript(name)(m1, m2))
  }
}
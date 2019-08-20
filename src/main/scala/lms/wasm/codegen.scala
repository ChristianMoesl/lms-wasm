package lms.wasm

import scala.collection.mutable
import java.io.{ByteArrayOutputStream, PrintStream}

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

  var lastNL = false
  def emit(s: String): Unit = { stream.print(s); lastNL = false }
  def emitln(s: String = "") = if (s != "" || !lastNL) { stream.println(s); lastNL = true }

  def array(innerType: String): String = ???
  def function(sig: List[Manifest[_]]): String = ???
  def nameMap: Map[String,String] = ???


  def record(man: lms.macros.RefinedManifest[_]): String = ???
  def remapUnsigned(m: Manifest[_]): String = ???

  def quote(s: Def): String = s match {
    case Sym(n) => s"x$n"
    case Const(s: String) => "\""+s.replace("\"", "\\\"")
      .replace("\n","\\n")
      .replace("\t","\\t")+"\\00\"" // TODO: more escapes?
    case Const(b: Boolean) => if (b) "1" else "0"
    case Const(x) => x.toString
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
    val lhs = stack.remove(0)
    val rhs = stack.remove(0)
    val f = typeFeature(rhs)

    assert(lhs == rhs, s"Type convertion is mandatory: $lhs != $rhs")

    emit(s"${remap(rhs)}.${remapBinOp(id, f.signed)}")

    if (comparisonop contains id)
      push(manifest[Boolean])
    else
      push(rhs)
  }

  def emitPrintf(): Unit = {
    if (symbolTable contains "printf" ) return

    emitPuts()
    emitMalloc()
    emitItoa()
    emitPuts()

    val maxIntStringLength = (math.ceil(math.log10(math.pow(2, 64))) + 2).toInt

    registerFunction("printf", List(manifest[Int]), None,
      s"""
        |(func $$printf (param $$n i32)
        |    (local $$str i32)
        |    (local $$len i32)
        |    (local.set $$str (call $$malloc (i32.const $maxIntStringLength)))
        |    (local.set $$len (call $$itoa (local.get $$n) (local.get $$str) (i32.const 10)))
        |    (i32.store8 (i32.add (local.get $$str) (local.get $$len)) (i32.const 10)) ;; add LF
        |    (local.set $$len (i32.add (local.get $$len) (i32.const 1)))
        |    (i32.store8 (i32.add (local.get $$str) (local.get $$len)) (i32.const 0))  ;; 0 terminated
        |
        |    (call $$puts (local.get $$str))
        |  )
        |""".stripMargin)
  }

  def emitPuts(): Unit = {
    if (symbolTable contains "puts") return

    emitStrLen()

    registerFunction("puts", List(manifest[String]), None,
      """
        |(func $puts (param $str i32)
        |    (i32.store (i32.const 0) (local.get $str))
        |    (i32.store (i32.const 4) (call $strlen (local.get $str)))
        |    (drop (call $fd_write
        |      (i32.const 1) ;; file_descriptor - 1 for stdout
        |      (i32.const 0) ;; *iovs - The pointer to the iov array, which is stored at memory location 0
        |      (i32.const 1) ;; iovs_len - We're printing 1 string stored in an iov - so one.
        |      (i32.const 8))) ;; nwritten - A place in memory to store the number of bytes writen
        |  )
        |""".stripMargin)
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

  def emitWasiAPIImports(): Unit = {
    registerDataBlock("wasi_api_params", 4 * 5)
    registerLibraryFunction("fd_write", List(manifest[Int], manifest[Int], manifest[Int], manifest[Int]), manifest[Int])
    registerLibraryFunction("args_sizes_get", List(manifest[Int], manifest[Int]), manifest[Int])
    registerLibraryFunction("args_get", List(manifest[Int], manifest[Int]), manifest[Int])
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

  def registerLibraryFunction(id: String, paramTypes: List[Manifest[_]], resultType: Manifest[_]) = {
    require(paramTypes.nonEmpty)

    symbolTable += (id -> Function(id, paramTypes, Some(resultType)))

    importSection.register(id, this) {
      val pTypes = paramTypes.map(t => remap(t)).mkString(" ")

      emitln(s"""(import "wasi_unstable" "$id" (func $$$id (param $pTypes) (result ${remap(resultType)})))""")
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

    val result = if (res != Const(())) {
      registerFunction(name, args.map(a => t(a)), Some(t(res)))
      s"(result ${remap(typeBlockRes(res))}) "
    } else {
      registerFunction(name, args.map(a => t(a)))
      ""
    }

    emitln(s"""(func $$$name (export "$name") ${args map(s => s"(param $$${quote(s)} ${remap(t(s))})") mkString(" ")} $result""")

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

  var controlLabel = 0
  override def traverse(n: Node): Unit = n match {
    case n @ Node(s, "var_new", List(exp), eff) =>
      emitLocalVariable(quote(s), t(s))
      shallow(exp); emitln()
      emitln(s"${scope(s)}.set $$${quote(s)}");
      pop()

    case n @ Node(s, "var_set", List(x, y), _) =>
      shallow(y); emitln()
      emitln(s"${scope(x)}.set $$${quote(x)}")
      pop()

    case n @ Node(s,"?",c::(a:Block)::(b:Block)::_,_) =>
      shallow(c); emitln()
      emitln(s"if ");
      pop()
      quoteBlock(traverse(a))
      quoteElseBlock(traverse(b))

    case n @ Node(f,"W",List(c:Block,b:Block),_) =>
      val block = allocateControlLabel()
      val loop = allocateControlLabel()
      emitln(s"block $$$block")
      emitln(s"loop $$$loop")
      quoteBlock(traverse(c))
      emitln(s"${remap(pop())}.eqz")
      emitln(s"br_if $$$block")
      traverse(b)
      emitln(s"br $$$loop")
      emitln("end")
      emitln("end")

    case n @ Node(_, "printf", List(Const("%d\n"), exp), _) =>
      emitPrintf()
      emitFunctionCall("printf") {
        shallow(exp)
      }

    case n @ Node(_, "printf", List(string), _) =>
      emitPuts()
      emitFunctionCall("puts") {
        shallow(string)
      }

    case Node(s, _, _, _) =>
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

  def t(e: lms.core.Backend.Exp): Manifest[_] = typeMap.getOrElse(e, manifest[Unknown])

  def shallow(n: Node): Unit = n match {

    case Node(s, "!", List(exp), _) =>
      shallow(exp); emitln()
      emitUnaryOperation("!"); emitln()
      assert(t(s) == stack.head, s"${t(s)} != ${stack.head}")

    case n @ Node(s, op, List(lhs, rhs), _) =>
      shallow(lhs); emitln()
      shallow(rhs); emitln()
      emitBinaryOperation(op); emitln()
      assert(t(s) == stack.head, s"${t(s)} != ${stack.head}")

    case n @ Node(s,"?",List(c, a: Block, b: Block),_) if a.isPure && a.res == Const(true) =>
      shallow(c)
      emitln("if (result i32)"); pop()
      shallow(a.res)
      emitln("else"); val ifType = pop()
      quoteBlock(traverse(b))
      emitln("end");
      assert(ifType == stack.head)

    case n @ Node(s,"?",List(c, a: Block, b: Block),_) if b.isPure && b.res == Const(false) =>
      shallow(c)
      emitln("if (result i32)"); pop()
      quoteBlock(traverse(a))
      emitln("else"); val ifType = pop()
      shallow(b.res);
      emitln("end");
      assert(ifType == stack.head)

    case Node(s,"var_get", List(x), _) =>
      emitln(s"${scope(x)}.get $$${quote(x)}")
      push(t(s))

    case _ =>
      println("============shallow(n: Node)")
      println(n)
      println("============")
  }

  def shallow(n: Def): Unit = n match {
    case InlineSym(n) => shallow(n)

    case n @ Sym(_) =>
      emitln(s"${scope(n)}.get $$${quote(n)}")
      push(t(n))

    case n @ Const(s: String) =>
      val offset = registerString(n)
      emitln(s"${remap(manifest[String])}.const $offset")
      push(manifest[String])

    case n @ Const(c) =>
      emitln(s"${remap(t(n))}.const ${quote(n)}")
      push(t(n))

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
    emitWasiAPIImports()
    emitMalloc()
    emitAtoi()
    emitPuts()

    memorySection.register("1", this) { emitln("(memory 1)") }
    exportSection.register("memory", this) { emitln("(export \"memory\" (memory 0))") }

    val paramOffset = symbolTable("wasi_api_params").asInstanceOf[DataBlock].offset

    val usageString = registerString("\"usage: ./prog <arg>\\n\\00\"")
    codeSection.register("main", this) {
      emitln(
        s"""
           |(func $$main (export "_start")
           |    (local $$argc i32)
           |    (local $$argv_buf_size i32)
           |    (local $$argv i32)
           |    (local $$argv_buf i32)
           |    (local $$arg i32)
           |
           |    (drop (call $$args_sizes_get (i32.const $paramOffset) (i32.const ${paramOffset + 4})))
           |
           |    (local.set $$argc (i32.load (i32.const $paramOffset)))
           |    (local.set $$argv_buf_size (i32.load (i32.const ${paramOffset + 4})))
           |
           |    (if (i32.lt_u (local.get $$argc) (i32.const 2))
           |      (then
           |        (call $$puts (i32.const $usageString))
           |        (return)
           |      )
           |    )
           |
           |    (local.set $$argv (call $$malloc (i32.mul (local.get $$argc) (i32.const 4))))
           |    (local.set $$argv_buf (call $$malloc (local.get $$argv_buf_size)))
           |
           |    (drop (call $$args_get (local.get $$argv) (local.get $$argv_buf)))
           |
           |    ;; get second argument
           |    (local.set $$arg (i32.load (i32.add (local.get $$argv) (i32.const 4))))
           |
           |    (call $$$call (call $$atoi (local.get $$arg)))
           |  )
           |""".stripMargin)
    }
  }

  override def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit = {
    val ng = init(g)

    emitln("(module")

    emitLibrary(name)

    val src = run(name, ng)
    codeSection.register("staged_code", this) {
      emit(src)
    }

    importSection.emit(to=stream)
    memorySection.emit(to=stream)
    globalSection.emit(to=stream)
    exportSection.emit(to=stream)
    codeSection.emit(to=stream)
    dataSection.emit(to=stream)

    emitln(")")
  }
}
package lms.wasm

import scala.collection.mutable
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}

import lms.core._
import lms.core.Backend._
import lms.macros.RefinedManifest

import scala.io.Source

class ExtendedWasmCodeGen extends CompactTraverser with ExtendedCodeGen {

  var lastNL = false
  def emit(s: String): Unit = { stream.print(s); lastNL = false }
  def emitln(s: String = "") = if (s != "" || !lastNL) {  stream.println(s); lastNL = true }

  class SymbolEntry(name: String)
  case class Variable(id: String, m: Manifest[_]) extends SymbolEntry(id)
  case class Function(id: String, paramTypes: List[Manifest[_]], resultType: Option[Manifest[_]]) extends SymbolEntry(id)
  case class StringLiteral(id: String, offset: Int) extends SymbolEntry(id)
  case class DataBlock(id: String, offset: Int, size: Int) extends SymbolEntry(id)
  case class RecordType(id: String, m: RefinedManifest[_]) extends SymbolEntry(id)

  private var symbolTable = new mutable.HashMap[String, SymbolEntry]

  class WasmSection(val id: Int) {
    private val nameMap = Array("custom", "type", "import", "function", "table", "memory", "global", "export", "start", "element", "code", "data")
    private val registered = mutable.HashSet[String]()
    private val stream = new ByteArrayOutputStream()
    private val writer = new PrintStream(stream)
    private var ongoingData = false

    def register(gen: ExtendedWasmCodeGen)(f: => Unit): Unit = {
      val save = gen.stream
      gen.stream = writer

      if (ongoingData) ???
      ongoingData = true
      f
      ongoingData = false

      gen.stream = save
    }

    def register(id: String, gen: ExtendedWasmCodeGen)(f: => Unit): Unit = if (!registered(id)) {
      registered += id
      register(gen)(f)
    }

    def emit(to: PrintStream) = {
      if (stream.size > 0) {
        to.println(s"\n;; *********** ${nameMap(id).capitalize} Section ***********")
        stream.writeTo(to)
      }
    }
  }

  private val importSection = new WasmSection(id = 2)
  private val memorySection = new WasmSection(id = 5)
  private val globalSection = new WasmSection(id = 6)
  private val exportSection = new WasmSection(id = 7)
  private val codeSection = new WasmSection(id = 10)
  private val dataSection = new WasmSection(id = 11)

  private var dataOffset = 0
  private val dataAlignment = 8

  override def array(innerType: String): String = "i32"
  override def function(sig: List[Manifest[_]]): String = ???
  override def nameMap: Map[String,String] = Map[String,String]()

  override def record(man: RefinedManifest[_]): String = {
    symbolTable += (man.toString -> RecordType(man.toString, man))
    "i32"
  }

  override def remapUnsigned(m: Manifest[_]): String = ???

  val rename = new mutable.HashMap[Sym,String]

  override def quote(s: Def): String = s match {
    case s @ Sym(n) => rename.getOrElseUpdate(s, s"$$x${rename.size}")
    case Const(c: Char) => c.toInt.toString
    case Const(b: Boolean) => if (b) "1" else "0"
    case Const(s: String) =>
      val escaped = "\""+s.replace("\"", "\\\"")
                        .replace("\n","\\n")
                        .replace("\t","\\t")+"\\00\""
      registerString(escaped).toString
    case Const(x: Array[Char]) =>
      val offset = registerDataBlock(ByteBuffer.allocate(x.length + 4)
                      .putInt(x.length).put(x.map(_.toByte)).array)
      (offset + 4).toString
    case Const(x) => x.toString
  }

  override def typeBlockRes(x: Exp): Manifest[_] = x match {
    case Const(x: Boolean) => manifest[Boolean]
    case _ => super.typeBlockRes(x)
  }

  override def primitive(rawType: String): String = rawType match {
    case "Boolean" => "i32"
    case "Char" => "i32"
    case "Int" => "i32"
    case "Long" => "i64"
    case "Float" => "f32"
    case "Double" => "f64"
    case "java.lang.String" => "i32"  // 32-bit pointer
    case _ => ???
  }

  def bitWidth(m: Manifest[_]): Int = m.toString match {
    case "lms.core.Unknown" => ???
    case "Boolean" => 32
    case "Char" => 8
    case "Int" => 32
    case "Long" => 64
    case "Float" => 32
    case "Double" => 64
    case "java.lang.String" => 8 // pointer
    case _ => 32  // reference
  }

  def emitFunctionCall(id: String, paramTypes: List[Manifest[_]], resultType: Option[Manifest[_]])(f: => Unit) {
    f
    emitln(s"call $$$id")

    for (n <- 1 to paramTypes.length)
      pop()

    if (resultType.isDefined)
      push(resultType.get)
  }

  val unaryop = Set("-","!","&")

  def remapUnaryOp(id: String) = id match {
    case "!" => "eqz"
    case "-" => ???
    case "&" => ???
  }

  def emitUnaryOperation(id: String): Unit = {
    val t = pop()

    emitln(s"${remap(t)}.${remapUnaryOp(id)}")

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
    def signed(m: Manifest[_]) =
      Set[Manifest[_]](manifest[Int], manifest[Long], manifest[Float], manifest[Double]) contains m

    val rhs = pop()
    val lhs = pop()

    assert(remap(lhs) == remap(rhs), s"type convertion is mandatory: $lhs != $rhs")
    assert(signed(lhs) == signed(rhs), s"types with different sign are used: $lhs(sign: ${signed(lhs)} != $rhs(sign: ${signed(rhs)})}")

    emitln(s"${remap(rhs)}.${remapBinOp(id, signed(rhs))}")

    if (comparisonop contains id)
      push(manifest[Boolean])
    else
      push(rhs)
  }

  private var stack = mutable.ListBuffer[Manifest[_]]()
  private def push(m: Manifest[_]): Unit = stack += m
  private def pop() = stack.remove(stack.length - 1)
  private def top = stack.last

  def registerGlobalVariable(id: String, m: Manifest[_], value: Int = 0): Unit = {
    require(m != manifest[Unknown])
    symbolTable += (id -> Variable(id, m))
    globalSection.register(id, this) {
      emitln(s"""(global $$$id (mut ${remap(m)}) (${remap(m)}.const $value))""")
    }
  }

  def roundUp(n: Int, steps: Int) = Math.ceil(n.toDouble / steps.toDouble).toInt * steps

  def registerDataBlock(data: mutable.IndexedSeq[Byte]): Int = {
    val offset = dataOffset
    val dataString = data.map(x => "\\%02x".format(x & 0xFF)).mkString("")
    dataSection.register(this) {
      emitln(s"""(data (i32.const $dataOffset) "$dataString")""")
    }
    dataOffset += roundUp(data.length, dataAlignment)
    offset
  }
  def registerDataBlock(id: String, data: mutable.IndexedSeq[Byte]): Int = {
    symbolTable += (id -> DataBlock(id, dataOffset, data.length))
    registerDataBlock(data)
  }

  def registerString(string: Const): Int = registerString(quote(string))
  def registerString(string: String): Int = {
    if (symbolTable contains string)
      return symbolTable(string).asInstanceOf[StringLiteral].offset

    symbolTable += (string -> StringLiteral(string, dataOffset))
    dataSection.register(string, this) {
      emitln(s"""(data (i32.const ${dataOffset}) $string)""")
    }
    val result = dataOffset
    dataOffset += roundUp(string.length, dataAlignment)
    result
  }

  def registerEnvironmentFunction(id: String, paramTypes: List[Manifest[_]], resultType: Option[Manifest[_]] = None): Unit = {
    if (symbolTable contains id) return

    require(paramTypes.nonEmpty)

    symbolTable += (id -> Function(id, paramTypes, resultType))

    importSection.register(id, this) {
      val pTypes = paramTypes.map(t => remap(t)).mkString(" ")

      val result = if (resultType.isDefined) s" (result ${remap(resultType.get)})" else ""

      emitln(s"""(import "env" "$id" (func $$$id (param $pTypes)$result))""")
    }
  }

  def emitEnvFunctionCall(id: String, paramTypes: List[Manifest[_]], resultType: Option[Manifest[_]] = None)(f: => Unit): Unit = {
    registerEnvironmentFunction(id, paramTypes, resultType)
    emitFunctionCall(id, paramTypes, resultType)(f)
  }

  def emitLibraryFunction(id: String, paramTypes: List[(String, Manifest[_])], resultType: Option[Manifest[_]] = None)(f: => Unit): Unit = {
    if (symbolTable contains id) return

    require(paramTypes.nonEmpty)

    symbolTable += (id -> Function(id, paramTypes.map(_._2), resultType))

    val pTypes = paramTypes.map(t => s"(param ${t._1} ${remap(t._2)})").mkString(" ")

    val result = if (resultType.isDefined) s" (result ${remap(resultType.get)})" else ""

    emitln(s"""(func $$$id $pTypes$result""")

    f

    emitln(")")
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
      emitln(s"(local $id ${remap(m)})")
    }
  }

  def emitFunction(name: String, body: Block): Unit = {
    val res = body.res
    val args = body.in

    emit(s"""(func $$$name (export "$name") ${args map(s => s"(param ${quote(s)} ${remap(t(s))})") mkString(" ")}""")
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

  private def emitVarOp(op: String, name: String, scope: String) = emitln(s"$scope.$op $name")

  private def emitVarTee(sym: Sym): Unit = emitVarTee(quote(sym), scope(sym))
  private def emitVarTee(name: String, scope: String) = emitVarOp("tee", name, scope)

  private def emitVarSet(sym: Sym): Unit = emitVarSet(quote(sym), scope(sym))
  private def emitVarSet(name: String, scope: String): Unit = { emitVarOp("set", name, scope); pop() }

  private def emitVarGet(sym: Sym): Unit = emitVarGet(quote(sym), scope(sym), t(sym))
  private def emitVarGet(name: String, scope: String, m: Manifest[_]): Unit  = { emitVarOp("get", name, scope); push(m) }

  private def emitLoad(m: Manifest[_]) = {
    pop()
    m.toString match {
      case "Char" => emitln("i32.load8_u")
      case "Boolean" => emitln("i32.load")
      case "Int" | "Long" | "Float" | "Double" => emitln(s"${remap(m)}.load")
      case "java.lang.String" => emitln("i32.load8_u")
      case _ => emitln("i32.load") // reference
    }
    push(m)
  }

  private def emitStore() = {
    val t = pop(); pop()
    t.toString match {
      case "Char" => emitln("i32.store8")
      case "Boolean" => emitln("i32.store")
      case "Int" | "Long" | "Float" | "Double" => emitln(s"${remap(t)}.store")
      case "java.lang.String" => emitln("i32.store8")
      case _ => emitln("i32.store")  // reference
    }
  }

  private def emitResultType(m: Manifest[_]) = emit(if (m == manifest[Unit]) "" else s" (result ${remap(m)})")

  private def emitConvert(to: Manifest[_]): Unit = {
    val from = pop()

    require(from != to)

    // trivial cases
    if (from == manifest[Char] && to == manifest[Int]) {
      push(to); return
    } else if (to == manifest[Boolean]) {
      if (Set[Manifest[_]](manifest[Int], manifest[Long], manifest[Float], manifest[Double], manifest[Char]) contains from) {
        emitln(s"${remap(from)}.eqz"); push(to); return
      } else ???
    }

    emit(s"${remap(to)}.")

    if (from == manifest[Boolean]) {
      if (to == manifest[Int] || to == manifest[Char]) ""
      else if (to == manifest[Long]) emit(s"extend_u/${remap(from)}")
      else if (to == manifest[Float] || to == manifest[Double]) emit(s"convert_u/${remap(from)}")
      else ???
    } else if (from == manifest[Int] || from == manifest[Long]) {
      if (to == manifest[Char]) {
        if (from == manifest[Long]) {
          emitln(s"wrap/${remap(from)}")
          emitln("i32.const 255")
        } else
          emitln("const 255")
        emitln("i32.and")
      }
      else if (from == manifest[Int] && to == manifest[Long]) emit(s"extend_s/${remap(from)}")
      else if (from == manifest[Long] && to == manifest[Int]) emit(s"wrap/${remap(from)}")
      else if (to == manifest[Float] || to == manifest[Double]) emit(s"convert_s/${remap(from)}")
      else ???
    } else if (from == manifest[Float] || from == manifest[Double]) {
      if (to == manifest[Char]) { emitln(s"trunc_u/${remap(from)}"); emitln("i32.const 255"); emitln("i32.and") }
      else if (from == manifest[Float] && to == manifest[Double]) emit(s"promote/${remap(from)}")
      else if (from == manifest[Double] && to == manifest[Float]) emit(s"demote/${remap(from)}")
      else if (to == manifest[Int] || to == manifest[Long]) emit(s"trunc_s/${remap(from)}")
      else ???
    } else if (from == manifest[Char]) {
      if (to == manifest[Long]) emit(s"extend_u/i32")
      else ???
    } else ???

    emitln()

    push(to)
  }

  private def emitIf(result: Manifest[_] = manifest[Unknown])(f: () => Unit) = emitIfThenElse(result)(f, None)
  private def emitIfThenElse(result: Manifest[_] = manifest[Unit])(thenF: () => Unit, elseF: Option[() => Unit]): Unit = {
    emit("if"); emitResultType(result); emitln(); pop()
    thenF()
    elseF.foreach { f =>
      emitln("else")
      f()
      if (result != manifest[Unit])
        assert(pop() == top)
    }
    emitln("end")
  }

  private def emitBr(label: String) = emitln(s"br $label")
  private def emitBrIf(label: String) = { emitln(s"br_if $label"); pop() }

  private var controlLabel = 0
  private def emitLoop(f: (String) => Unit) = emitBlockKind("loop")(f)
  private def emitBlock(f: (String) => Unit) = emitBlockKind("block")(f)
  private def emitBlockKind(kind: String)(f: (String) => Unit): Unit = {
    val label = s"$$$controlLabel"
    controlLabel += 1
    emitln(s"$kind $label")
    f(label)
    emitln("end")
    controlLabel -= 1
  }

  private def emitArrayElementAddress(arr: Def, idx: Def): Manifest[_] = {
    val innerType = t(arr).typeArguments match {
      case List(inner) => inner
      case _ => ???
    }

    val byteWidth = bitWidth(innerType) / 8

    shallow(arr); pop(); push(manifest[Int])
    shallow(idx)
    if (byteWidth > 1) {
      shallow(Const(byteWidth))
      emitBinaryOperation("*")
    }
    emitBinaryOperation("+")

    innerType
  }

  override def traverse(n: Node): Unit = n match {

    // Variables
    case Node(s, "var_new", List(exp), _) =>
      emitLocalVariable(quote(s), t(s))
      shallow(exp)
      emitVarSet(s)

    case Node(_, "var_set", List(x: Sym, y), _) =>
      shallow(y)
      emitVarSet(x)

    // Control flow
    case Node(s, "?", c::(a:Block)::(b:Block)::_, _) if !dce.live(s) =>
      shallow(c)
      emitIfThenElse()(() => quoteBlock(traverse(a)), Some(() => quoteBlock(traverse(b))))

    case Node(_, "W", List(c:Block,b:Block), _) =>
      emitBlock { block => emitLoop { loop =>
        quoteBlock(traverse(c))
        emitln(s"${remap(top)}.eqz")
        emitBrIf(block)
        traverse(b)
        emitBr(loop)
      }}

    case Node(_, "switch", (guard: Exp)::default::others, _) =>
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
    case Node(_,"generate-comment", List(Const(x: String)),_) =>
      emit(";; "); emitln(x)

    case Node(s,"comment",Const(str: String)::Const(verbose: Boolean)::(b:Block)::_,_) =>
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

    case _ => // val defintion
      val prevSize = stack.size
      shallow(n)
      if (dce.live(n.n)) {
        emitLocalVariable(quote(n.n), t(n.n))
        emitVarSet(n.n)
      } else if (stack.size > prevSize) {
        emitln("drop"); pop()
      }
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

  def t(e: Def): Manifest[_] =
    if (e.isInstanceOf[Exp]) typeBlockRes(e.asInstanceOf[Exp])
    else manifest[Unknown]

  var tmpCount = 0
  def emitTmpVariable(m: Manifest[_])(f: (String) => Unit): Unit = {
    val id = s"$$t$tmpCount"
    emitLocalVariable(id, m);
    tmpCount += 1
    f(id)
  }

  def typeMask(args: List[Exp]): Int = {
    def typeMaskValue(m: Manifest[_]): Int = {
      if (m == manifest[Boolean]) 1
      else if (m == manifest[Char]) 2
      else if (m == manifest[String]) 3
      else if (m == manifest[Int]) 4
      else if (m == manifest[Long]) 5
      else if (m == manifest[Float]) 6
      else if (m == manifest[Double]) 7
      else ???
    }
    args.map(a => typeMaskValue(t(a))).reverse.fold(0)((acc,y) => (acc << 8) + y)
  }

  def storeEnvPrintParam(arg: (Exp, Int)) = {
    val offset = symbolTable("_printParams").asInstanceOf[DataBlock].offset
    shallow(Const(offset + 8 * arg._2))
    shallow(arg._1)
    emitStore()
  }

  def convertToEnvParamTypes(args: List[Exp]) = args.map { x =>
    if (t(x) == manifest[Long]) manifest[Int]
    else if (t(x) == manifest[Double]) manifest[Float]
    else t(x)
  }

  def shallow(n: Node): Unit = n match {

    case Node(s, "var_get", List(x), _) =>
      if (dce.live(s)) shallow(x)

    case Node(s, "!", List(exp), _) =>
      shallow(exp)
      emitUnaryOperation("!")
      assert(t(s) == top, s"${t(s)} != $top")

    case Node(_, op, List(lhs, rhs), _) if binop contains op =>
      shallow(lhs)
      shallow(rhs)
      emitBinaryOperation(op)

    case Node(s, "cast", List(x, y), _) =>
      assert(t(s).toString == y.toString)
      shallow(x)
      emitConvert(t(s))

    case Node(f, "?", c::(a:Block)::(b:Block)::_, _) =>
      shallow(c)
      emitIfThenElse(t(f))(() => quoteBlock(traverse(a)), Some(() => quoteBlock(traverse(b))))

    // String methods
    case n @ Node(_, op, args, _) if op.startsWith("String") =>
      (op.substring("String.".length), args) match {
        case ("slice", List(lhs, start, end)) =>
          emitEnvFunctionCall("stringSlice", List(manifest[String], manifest[Int], manifest[Int]), Some(manifest[String])) {
            shallow(lhs); shallow(start); shallow(end)
          }
        case ("toDouble", List(rhs)) =>
          emitEnvFunctionCall("stringToDouble", List(manifest[String]), Some(manifest[Float])) {
            shallow(rhs)
          }
        case ("toInt", List(rhs)) =>
          emitEnvFunctionCall("stringToInt", List(manifest[String]), Some(manifest[Int])) {
            shallow(rhs)
          }
        case ("length", List(rhs)) =>
          emitEnvFunctionCall("stringLength", List(manifest[String]), Some(manifest[Int])) {
            shallow(rhs)
          }
        case ("charAt", List(lhs, i)) =>
          emitEnvFunctionCall("stringCharAt", List(manifest[String], manifest[Int]), Some(manifest[Char])) {
            shallow(lhs); shallow(i)
          }
        case default => throw new NotImplementedError(n.toString)
      }

    // Array
    case Node(s, "NewArray", List(x) ,_) =>
      val size = t(s).typeArguments match {
        case List(inner) => bitWidth(inner) / 8
        case _ => ???
      }

      emitTmpVariable(manifest[Int]) { len =>
        shallow(x)
        emitVarTee(len, "local")

        if (size > 1) {
          shallow(Const(size.toInt))
          emitBinaryOperation("*")
        }
        shallow(Const(4)) // to safe length
        emitBinaryOperation("+")

        emitEnvFunctionCall("malloc", List(manifest[Int]), Some(manifest[Int])) { }

        emitTmpVariable(manifest[Int]) { ptr =>
          emitVarTee(ptr, "local")
          emitVarGet(len, "local", manifest[Int])
          emitStore()

          emitVarGet(ptr, "local", manifest[Int])
          shallow(Const(4))
          emitBinaryOperation("+")
        }

        pop(); push(t(s))
      }

    case Node(s, "array_get", List(arr, idx), _) =>
      if (dce.live(s)) emitLoad(emitArrayElementAddress(arr, idx))

    case Node(s, "array_set", List(arr, idx, y), _) =>
      emitArrayElementAddress(arr, idx)
      shallow(y)
      emitStore()

    case Node(_, "array_length", List(arr), _) =>
      shallow(arr); pop(); push(manifest[Int])
      shallow(Const(4))
      emitBinaryOperation("-")
      emitLoad(manifest[Int])

    // MiscOps
    case Node(_, "StrSubHashCode", List(str, len), _) =>
      emitFunctionCall("_hash", List(manifest[String], manifest[Int]), Some(manifest[Long])) {
        shallow(str)
        shallow(len)
      }

    case Node(_, "P", args: List[_], _) if (0 to 4 contains args.length) =>
      val a = args.asInstanceOf[List[Exp]]
      emitEnvFunctionCall(s"println", List(manifest[Int])) {
        shallow(Const(typeMask(a)))
        a.zipWithIndex foreach storeEnvPrintParam
      }

    case Node(_, "printf", args: List[_], _) if (1 to 5 contains args.length) =>
      val a = args.asInstanceOf[List[Exp]]
      emitEnvFunctionCall(s"printf", List(manifest[String], manifest[Int])) {
        shallow(a.head)
        shallow(Const(typeMask(a.tail)))
        a.asInstanceOf[List[Exp]].tail.zipWithIndex foreach storeEnvPrintParam
      }

    case Node(_, "printData", List(data, len), _) =>
      emitEnvFunctionCall("printData", List(manifest[Int], manifest[Int])) {
        shallow(data)
        shallow(len)
      }

    case Node(_, "readFile", List(name), _) =>
      emitEnvFunctionCall("readFile", List(manifest[String]), Some(manifest[Array[Char]])) {
        shallow(name)
      }

    case Node(_, "open", List(name), _) =>
      emitEnvFunctionCall("open", List(manifest[String]), Some(manifest[Int])) {
        shallow(name)
      }

    case Node(_, "close" , List(fd), _) =>
      emitEnvFunctionCall("close", List(manifest[Int])) {
        shallow(fd)
      }

    case n @ Node(_, op, _, _) if nameMap contains op =>
      shallow(n.copy(op = nameMap(n.op)))

    case Node(_, op, args, _) if op.startsWith("unchecked") =>
      val stackSize = stack.size
      var next = "unchecked".length
      var argc = 0
      while (next < op.length && (op(next) == '(' || op(next) == '[')) {
        if (op(next) == '[') {
          shallow(args(argc));
          argc += 1; next += 3
        } else {
          val i = op.indexOf(")", next)
          emitln(op.substring(next + 1, i))
          next = i + 1
        }
      }
      for (_ <- 1 to (stack.size - stackSize)) pop()
      push(manifest[Int])

    case _ => throw new NotImplementedError(n.toString)
  }

  def shallow(n: Def): Unit = n match {
    case InlineSym(n) => shallow(n)
    case n @ Sym(_) => emitln(s"${scope(n)}.get ${quote(n)}"); push(t(n))
    case n @ Const(_) => emitln(s"${remap(t(n))}.const ${quote(n)}"); push(t(n))
    case _ => throw new NotImplementedError(n.toString)
  }

  def run(name: String, g: Graph) = {
    capture {
      bound(g)
      withScope(Nil, g.nodes) {
        emitFunction(name, g.block)
      }
    }
  }

  def emitLibrary: Unit = {

    registerDataBlock("_printParams", new Array[Byte](4 * (bitWidth(manifest[Long]) / 8)))

    val stream = capture {
      //long hash(char *str0, int len) {
      //  unsigned char* str = (unsigned char*)str0;
      //  unsigned long hash = 5381;
      //  int c;
      //  while ((c = *str++) && len--)
      //    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
      //  return hash;
      //}
      val str = "$str"
      val len = "$len"
      emitLibraryFunction("_hash", List((str, manifest[Int]), (len, manifest[Int])), Some(manifest[Long])) {
        val c = "$t0"
        val hash = "$t1"

        emitln(s"(local $c i32)")
        emitln(s"(local $hash i64)")
        shallow(Const(5381.toLong))
        emitVarSet(hash, "local")
        emitVarGet(str, "local", manifest[String])
        emitLoad(manifest[Char])
        emitVarSet(c, "local")
        emitBlock { block =>
          emitLoop { loop =>
            emitVarGet(c, "local", manifest[Int])
            emitln("i32.eqz")
            emitBrIf(block)
            emitVarGet(len, "local", manifest[Int])
            emitln("i32.eqz")
            emitBrIf(block)

            emitVarGet(hash, "local", manifest[Long])
            shallow(Const(5.toLong))
            emitBinaryOperation("<<")
            emitVarGet(hash, "local", manifest[Long])
            emitBinaryOperation("+")
            emitVarGet(str, "local", manifest[Int])
            emitLoad(manifest[Char])
            emitConvert(manifest[Long])
            emitBinaryOperation("+")
            emitVarSet(hash, "local")

            emitVarGet(str, "local", manifest[Int])
            shallow(Const(1))
            emitBinaryOperation("+")
            emitVarTee(str, "local")
            emitLoad(manifest[Char]);
            emitVarSet(c, "local")

            emitVarGet(len, "local", manifest[Int])
            shallow(Const(1))
            emitBinaryOperation("-")
            emitVarSet(len, "local");

            emitBr(loop)
          }
        }
        emitVarGet(hash, "local", manifest[Long]); pop()
      }
    } ensuring stack.isEmpty

    codeSection.register("_hash", this) {
      emitln(stream.toString)
    }

    memorySection.register("1", this) { emitln("(memory (export \"mem\") 1)") }
  }

  def generateJavaScript(name: String)(m1:Manifest[_],m2:Manifest[_]): String = {
    val paramTypes = symbolTable(name).asInstanceOf[Function].paramTypes

    def args(sep: String) = paramTypes.indices map (n => s"arg$n") mkString sep

    def toJsType(m: Manifest[_]) = if (m == manifest[Int] || m == manifest[Boolean]) "Int" else "Float"

    def parseArg(i: Int, m: Manifest[_]) =
      if (m1 == manifest[Boolean])
        s"""const arg$i = (args[$i] === "true") ? true :
             (args[$i] === "false") ? false : undefined;"""
      else if (m1 == manifest[String])
        ""
      else if (m1 == manifest[Long])
        s"let arg$i; try { arg$i = BigInt(args[$i]); } catch (e) { arg$i = undefined; }"
      else {
        val jsType = toJsType(if (m1 == manifest[Double]) manifest[Float] else m1)
        s"const arg$i = Number.parse$jsType(args[$i]);"
      }

    val parseArgs = paramTypes.zipWithIndex.map(a => parseArg(a._2, a._1)).mkString("\n")

    val checkArgs =
      if (m1 == manifest[Boolean])
        s"if (${paramTypes.indices map (i => s"arg$i === undefined") mkString " || "})"
      else if (m1 == manifest[String])
        "if (false)"
      else
        s"if (${paramTypes.indices map (i => s"isNaN(arg$i)") mkString " || "})"

    val insertString = if (m1 != manifest[String]) "" else
      s"""const arg0 = $dataOffset;""" + "\n" + paramTypes.indices
        .map(i => s"""const arg${i + 1} = insertAt(new Uint8Array(mem.buffer), arg$i, args[$i]);""")
        .mkString("\n")

    val usageString = s"""const s = `usage: ${args(", ")}`;"""

    val functonCall = s""".$name(${args(", ")})"""

    Source.fromResource("environment.js").getLines().mkString("\n")
      .replace("/*PARSE_ARGS*/", parseArgs)
      .replace("/*CHECK_ARGS*/", checkArgs)
      .replace("/*USAGE_STRING*/", usageString)
      .replace("/*INSERT_STRINGS*/", insertString)
      .replace("/*FUNCTION_CALL*/", functonCall)
  }

  override def emitAll(g: Graph, name: String)(m1:Manifest[_], m2:Manifest[_]): Unit = {
    require(m1 != manifest[Long], "i64 is not allowed in WASM function in V8")
    require(m2 == manifest[Unit], "second argument is currently not supported")

    val ng = init(g)

    emitln("(module")

    emitLibrary

    val src = run(name, ng)
    codeSection.register(this) {
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
package lms

import java.io._

import lms.core.utils
import org.scalatest.FunSuite

trait LibSuite extends FunSuite {
  def dataFilePath(csv: String) = "src/data/" + csv
}

trait TutorialFunSuite extends LibSuite {
  val overwriteCheckFiles = false // should be false; temporary set to true only to simplify development

  val prefix = "src/out/"
  val under: String
  override protected def test(testName: String,testTags: org.scalatest.Tag*)(testFun: => Any)(implicit pos: org.scalactic.source.Position): Unit = {
    super.test(testName,testTags:_*)(utils.time(under)(utils.time(testName)(testFun)))(pos)
  }
  def readFile(name: String): String = {
    try {
      val buf = new Array[Byte](new File(name).length().toInt)
      val fis = new FileInputStream(name)
      fis.read(buf)
      fis.close()
      new String(buf)
    } catch {
      case e: IOException => ""
    }
  }
  def writeFile(name: String, content: String) {
    val out = new java.io.PrintWriter(new File(name))
    out.write(content)
    out.close()
  }
  def writeFileIndented(name: String, content: String) {
    val out = new java.io.PrintWriter(new File(name))
    printIndented(content)(out)
    out.close()
  }
  def checkSnippet(label: String, thunk: => Unit) = {
    val output = utils.captureOut(try thunk catch { case e: Throwable => e.printStackTrace })
    check(label, output, checkLibrary = false)
  }
  def checkOut(label: String, thunk: => Unit) = {
    val output = utils.captureOut(try thunk catch { case e: Throwable => e.printStackTrace })
    check(label, output)
  }
  def check(label: String, raw_code: String, checkLibrary: Boolean = true) = {
    val fileprefix = prefix+under+label
    val name = fileprefix+".check.wat"
    val aname = fileprefix+".actual.wat"
    val expected = if (checkLibrary) readFile(name) else removeBoilerplate(readFile(name))
    val code = indent(if (checkLibrary) raw_code else removeBoilerplate(raw_code))
    if (expected != code) {
      val wname = if (overwriteCheckFiles) name else aname
      println("writing " + wname)
      writeFile(wname, code)
    } else {
     val f = new File(aname)
    // if (f.exists) f.delete
   }
    if (!overwriteCheckFiles) {
      assert(expected == code, name)
    }
  }
  def removeBoilerplate(code: String): String = {
    val lines = code.split("[\n\r]")
    var b = new StringBuilder

    var output = false
    var indent = 0

    for (line <- lines) {
      if (line.contains(";; output:") || output) {
        output = true
        b ++= line + '\n'
      } else if (line.contains("(func $Snippet ") && indent == 0) {
        indent += 1
        b ++= line + '\n'
      } else if (indent > 0) {
        indent += line.chars().mapToLong{ case '(' => 1; case ')' => -1; case _ => 0 }.sum().toInt
        b ++= line + '\n'
      }
    }
    b.toString
  }
  def indent(str: String) = {
    val s = new StringWriter
    printIndented(str)(new PrintWriter(s))
    s.toString
  }
  def printIndented(str: String)(out: PrintWriter): Unit = {
    val lines = str.split("[\n\r]")
    var indent = 0
    for (l0 <- lines) {
      val l = l0.trim
      if (l.length > 0) {
        var open = 0
        var close = 0
        var initClose = 0
        var nonWsChar = false
        l.trim match {
          case s if s startsWith "br_if" =>
          case s if s startsWith "if" => open += 1
          case s if s startsWith "block" => open += 1
          case s if s startsWith "loop" => open += 1
          case s if s startsWith "then" => close += 1
          case s if s startsWith "else" =>
            open += 1
            close += 1
          case s if s startsWith "end" => close += 1
          case _ =>
        }
        l foreach {
          case '(' =>  {
            open += 1
          }
          case ')' =>  {
            close += 1
          }
          case x => if (!nonWsChar && !x.isWhitespace) {
            nonWsChar = true
            initClose = close
          }
        }
        if (!nonWsChar) initClose = close
        out.println("  " * (indent - initClose) + l)
        indent += (open - close)
      }
    }
//    assert (indent==0, "indentation sanity check")
  }

  def exec(label: String, code: String, suffix: String = "scala") = {
    val fileprefix = prefix+under+label
    val aname = fileprefix+".actual."+suffix
    writeFileIndented(aname, code)
  }
}

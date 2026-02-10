#!/usr/bin/env scala-cli

//> using scala 3.6
//> using dep com.lihaoyi::os-lib:0.9.3

// This script fixes multi-line parameter list formatting in Scala files.
// It only processes lines that are clearly the START of a parameter/type-parameter block.
//
// A line is considered the start of a parameter block if:
// 1. The line starts with ( or [ (after leading whitespace)
// 2. The previous line is a declaration that doesn't already have an open bracket
//    OR the previous line ends with ] and this line starts with (
//
// This is conservative to avoid false positives.

def isDeclarationKeyword(line: String): Boolean =
  val trimmed = line.dropWhile(_ == ' ')
  trimmed.startsWith("def ") || trimmed.startsWith("class ") ||
  trimmed.startsWith("case class ") || trimmed.startsWith("trait ") ||
  trimmed.startsWith("given ") || trimmed.startsWith("inline ") ||
  trimmed.startsWith("transparent ") || trimmed.startsWith("enum ") ||
  trimmed.startsWith("extension ")

def getBaseIndent(line: String): Int =
  line.takeWhile(_ == ' ').length

def findDeclarationAbove(lines: IndexedSeq[String], lineIndex: Int): Option[(Int, String)] =
  // Look backwards to find a declaration, skipping only annotations and bracket lines
  var i = lineIndex - 1
  while i >= 0 do
    val line = lines(i)
    val trimmed = line.trim
    
    if trimmed.isEmpty then
      i -= 1
    else if isDeclarationKeyword(line) then
      return Some((getBaseIndent(line), line))
    else if trimmed.startsWith("@") then
      // Annotation - keep looking
      i -= 1
    else if trimmed.startsWith("(") || trimmed.startsWith("[") then
      // Another bracket line - keep looking
      i -= 1
    else
      // Hit something else - not a declaration parameter list
      return None
  None

def isStartOfParamBlock(line: String, prevLine: String, lines: IndexedSeq[String], lineIndex: Int): Option[Int] =
  val trimmed = line.dropWhile(_ == ' ')
  val prevTrimmed = prevLine.trim
  
  // Skip lambda expressions
  if trimmed.contains("=>") then return None
  
  // Case 1: Previous line is a declaration without open bracket
  // e.g., "def foo" or "def foo[T]" followed by "(param: Type)"
  if isDeclarationKeyword(prevLine) then
    // Declaration shouldn't end with = : { or already have ( for value params
    if prevTrimmed.endsWith("=") || prevTrimmed.endsWith(":") || 
       prevTrimmed.endsWith("{") then
      return None
    // Special case: "given TypeName" without colon is an anonymous given, not a declaration with params
    // e.g., "given Decimalizer" followed by "(args)" is a constructor call, not param list
    if prevTrimmed.startsWith("given ") && !prevTrimmed.contains(":") then
      return None
    return Some(getBaseIndent(prevLine))
  
  // Case 2: Previous line ends with ] (type params closed), this line starts with (
  // e.g., "def foo[T]" followed by "(param: Type)"
  // BUT we need to verify there's a declaration above
  if prevTrimmed.endsWith("]") && trimmed.startsWith("(") then
    findDeclarationAbove(lines, lineIndex) match
      case Some((indent, declLine)) =>
        // Make sure the declaration line doesn't contain = (which would mean it's a body)
        if !declLine.contains(" = ") && !declLine.trim.endsWith("=") then
          return Some(indent)
      case None => ()
  
  // Case 3: Previous line ends with [ (type params open), this line continues them
  if prevTrimmed.endsWith("[") && trimmed.startsWith("[") then
    findDeclarationAbove(lines, lineIndex) match
      case Some((indent, _)) => return Some(indent)
      case None => ()
  
  // Case 4: Previous line ends with ( (value params open), this line continues them
  // e.g., "def foo(" followed by "(using ctx: Context)"
  if prevTrimmed.endsWith("(") && trimmed.startsWith("(") then
    findDeclarationAbove(lines, lineIndex) match
      case Some((indent, _)) => return Some(indent)
      case None => ()
  
  None

def processLine(line: String, lines: IndexedSeq[String], lineIndex: Int): String =
  val trimmed = line.dropWhile(_ == ' ')
  
  // Only process lines starting with ( or [
  if !trimmed.startsWith("(") && !trimmed.startsWith("[") then
    return line
  
  if lineIndex == 0 then return line
  val prevLine = lines(lineIndex - 1)
  
  val baseIndent = isStartOfParamBlock(line, prevLine, lines, lineIndex) match
    case Some(indent) => indent
    case None => return line
  
  val currentIndent = getBaseIndent(line)
  val bracket = trimmed.head
  val closeBracket = if bracket == '(' then ')' else ']'
  
  val targetIndent = baseIndent + 2
  
  var result = trimmed
  
  // Add space after opening bracket if needed
  if result.length > 1 && result(1) != ' ' && result(1) != closeBracket then
    result = s"$bracket ${result.tail}"
  
  // Add space before closing bracket if the line ends with one
  val closeIdx = result.lastIndexOf(closeBracket)
  if closeIdx > 0 && result(closeIdx - 1) != ' ' then
    // Only add space if this closing bracket is at or near the end
    val afterClose = result.substring(closeIdx + 1)
    if afterClose.isEmpty || afterClose == ":" || afterClose == " =" || 
       afterClose.startsWith(")") || afterClose.startsWith("]") then
      result = result.substring(0, closeIdx) + s" $closeBracket" + afterClose
  
  val newLine = " " * targetIndent + result
  
  if newLine != line then newLine else line

def processFile(path: os.Path): Boolean =
  val content = os.read(path)
  val lines = content.split("\n", -1).toIndexedSeq
  
  var changed = false
  val newLines = lines.zipWithIndex.map { case (line, idx) =>
    val newLine = processLine(line, lines, idx)
    if newLine != line then
      changed = true
    newLine
  }
  
  if changed then
    os.write.over(path, newLines.mkString("\n"))
    println(s"Fixed: $path")
  
  changed

@main def main(args: String*): Unit =
  val path = if args.isEmpty then os.pwd else os.Path(args.head, os.pwd)
  
  var count = 0
  
  if os.isFile(path) then
    if processFile(path) then count += 1
  else
    os.walk(path)
      .filter(_.ext == "scala")
      .filter(p => !p.toString.contains("/target/") && !p.toString.contains("/out/"))
      .foreach { p =>
        if processFile(p) then count += 1
      }
  
  println(s"Fixed $count files")

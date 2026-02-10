

//> using scala 3.6
//> using dep com.lihaoyi::os-lib:0.9.3

// This script aligns parameters in multi-line parameter blocks.
// Style:
//   ( name:      Type           = default,
//     otherName: LongerType     = otherDefault )
//
// Alignment rules:
// - Colon immediately after name (name:)
// - Spaces after colon to align type column
// - If defaults exist: spaces after type to align = signs
// - Continuation lines indented to align with first parameter content

case class Parameter(
  prefix: String,      // e.g., "val ", "inline ", "using ", ""
  name: String,        // parameter name
  typePart: String,    // the type
  hasDefault: Boolean, // whether there's a default value
  defaultValue: String // the default value
)

def parseParameter(content: String): Option[Parameter] =
  val trimmed = content.trim
  if trimmed.isEmpty then return None
  
  // Skip lines that are just closing brackets or whitespace
  if trimmed == ")" || trimmed == "]" || trimmed == "):" || 
     trimmed == ") =" || trimmed == " )" || trimmed == " ]" ||
     trimmed.startsWith(")") || trimmed.startsWith("]") then
    return None
  
  var rest = trimmed
  var prefix = ""
  
  // Extract prefix (val, inline, using, etc.)
  if rest.startsWith("val ") then
    prefix = "val "
    rest = rest.drop(4)
  else if rest.startsWith("inline val ") then
    prefix = "inline val "
    rest = rest.drop(11)
  else if rest.startsWith("inline ") then
    prefix = "inline "
    rest = rest.drop(7)
  else if rest.startsWith("transparent inline ") then
    prefix = "transparent inline "
    rest = rest.drop(19)
  else if rest.startsWith("transparent ") then
    prefix = "transparent "
    rest = rest.drop(12)
  else if rest.startsWith("using ") then
    prefix = "using "
    rest = rest.drop(6)
  
  // Find the colon (but not ::)
  var colonIdx = -1
  var i = 0
  while i < rest.length && colonIdx < 0 do
    if rest(i) == ':' && (i + 1 >= rest.length || rest(i + 1) != ':') then
      colonIdx = i
    i += 1
  
  if colonIdx < 0 then return None
  
  val name = rest.take(colonIdx).trim
  if name.isEmpty || name.contains("(") || name.contains(")") || name.contains("[") then
    return None
  
  rest = rest.drop(colonIdx + 1).stripLeading()
  
  // Find equals sign for default value (but not =>, =:=, or inside brackets)
  var bracketDepth = 0
  var equalsIdx = -1
  i = 0
  while i < rest.length && equalsIdx < 0 do
    rest(i) match
      case '[' | '(' | '{' => bracketDepth += 1
      case ']' | ')' | '}' => bracketDepth -= 1
      case '=' if bracketDepth == 0 =>
        // Check it's not => or =:= or other operators
        val nextChar = if i + 1 < rest.length then rest(i + 1) else ' '
        val prevChar = if i > 0 then rest(i - 1) else ' '
        if nextChar != '>' && nextChar != ':' && prevChar != ':' && prevChar != '!' && prevChar != '<' && prevChar != '>' then
          equalsIdx = i
      case _ =>
    i += 1
  
  var typePart: String = ""
  var hasDefault = false
  var defaultValue = ""
  
  // Helper to find where the value/type ends (comma or end-of-block bracket at depth 0)
  def findEndIdx(s: String): Int =
    var depth = 0
    var idx = s.length
    var j = 0
    while j < s.length do
      s(j) match
        case '[' | '(' | '{' => depth += 1
        case ']' | ')' | '}' if depth > 0 => depth -= 1
        case ',' if depth == 0 => idx = j; j = s.length
        case ')' | ']' if depth == 0 => idx = j; j = s.length
        case ' ' if depth == 0 && j + 1 < s.length && (s(j + 1) == ')' || s(j + 1) == ']') =>
          idx = j; j = s.length
        case _ =>
      j += 1
    idx
  
  if equalsIdx >= 0 then
    typePart = rest.take(equalsIdx).trim
    hasDefault = true
    rest = rest.drop(equalsIdx + 1).stripLeading()
    val endIdx = findEndIdx(rest)
    defaultValue = rest.take(endIdx).trim
  else
    val endIdx = findEndIdx(rest)
    typePart = rest.take(endIdx).trim
  
  Some(Parameter(prefix, name, typePart, hasDefault, defaultValue))

def formatParameters(params: List[Parameter], baseIndent: Int, openBracket: Char, closeBracket: Char): List[String] =
  if params.isEmpty then return Nil
  
  // Calculate column widths
  val maxPrefixLen = params.map(_.prefix.length).max
  val maxNameLen = params.map(_.name.length).max
  val maxTypeLen = params.map(_.typePart.length).max
  
  val hasAnyDefault = params.exists(_.hasDefault)
  
  params.zipWithIndex.map { case (p, idx) =>
    val isLast = idx == params.length - 1
    val indent = " " * baseIndent
    val contentIndent = " " * (baseIndent + 2)
    
    val prefixPadded = p.prefix.padTo(maxPrefixLen, ' ')
    val nameColonPadded = (p.name + ":").padTo(maxNameLen + 1, ' ')
    
    val typePadded = if hasAnyDefault then p.typePart.padTo(maxTypeLen, ' ') else p.typePart
    
    val lineStart = if idx == 0 then
      s"$indent$openBracket $prefixPadded$nameColonPadded $typePadded"
    else
      s"$contentIndent$prefixPadded$nameColonPadded $typePadded"
    
    val suffix = if isLast then s" $closeBracket" else ","
    
    if p.hasDefault then
      s"$lineStart = ${p.defaultValue}$suffix"
    else
      s"${lineStart.stripTrailing()}$suffix"
  }

case class ParamBlock(
  startLine: Int,
  endLine: Int,
  baseIndent: Int,
  openBracket: Char,
  closeBracket: Char,
  params: List[Parameter],
  trailingContent: String
)

def findParamBlocks(lines: IndexedSeq[String]): List[ParamBlock] =
  var blocks = List.empty[ParamBlock]
  var i = 0
  
  while i < lines.length do
    val line = lines(i)
    val trimmed = line.trim
    
    // Look for lines starting with ( or [ that might be parameter blocks
    if (trimmed.startsWith("( ") || trimmed.startsWith("[ ")) && trimmed.contains(":") then
      val baseIndent = line.takeWhile(_ == ' ').length
      val openBracket = trimmed.head
      val closeBracket = if openBracket == '(' then ')' else ']'
      
      // Track bracket depth more carefully - start at 1 for the opening bracket
      var bracketDepth = 1
      var j = i
      var trailingContent = ""
      var blockLines = List.empty[String]
      
      // Process first line content (after "( " or "[ ")
      val firstContent = trimmed.drop(2)
      blockLines = blockLines :+ firstContent
      
      // Update depth for first line (skip the first char which is part of "( ")
      firstContent.foreach {
        case c if c == openBracket => bracketDepth += 1
        case c if c == closeBracket => bracketDepth -= 1
        case _ =>
      }
      
      j += 1
      
      // Continue until we close the block
      while j < lines.length && bracketDepth > 0 do
        val nextLine = lines(j)
        val nextTrimmed = nextLine.trim
        
        blockLines = blockLines :+ nextTrimmed
        
        nextTrimmed.foreach {
          case c if c == openBracket => bracketDepth += 1
          case c if c == closeBracket => bracketDepth -= 1
          case _ =>
        }
        
        j += 1
      
      // Extract trailing content from last line (after the final closing bracket)
      if blockLines.nonEmpty then
        val lastLine = blockLines.last
        val closeIdx = lastLine.lastIndexOf(closeBracket)
        if closeIdx >= 0 && closeIdx < lastLine.length - 1 then
          trailingContent = lastLine.drop(closeIdx + 1)
      
      // Parse parameters from collected lines
      var params = List.empty[Parameter]
      var hasTypeContinuation = false
      
      blockLines.foreach { content =>
        val trimmed = content.trim
        // Check if this is a type continuation (no colon for parameter name)
        // Skip lines that are just closing brackets
        if trimmed.nonEmpty && !trimmed.startsWith(")") && !trimmed.startsWith("]") then
          if !trimmed.contains(":") || (trimmed.indexOf(":") > 0 && trimmed.take(trimmed.indexOf(":")).contains(" ")) then
            // This might be a type continuation line (like "to SomeType,")
            // unless it's a valid parameter with prefix
            val hasValidPrefix = trimmed.startsWith("val ") || trimmed.startsWith("using ") || 
                                 trimmed.startsWith("inline ") || trimmed.startsWith("transparent ")
            if !hasValidPrefix && !trimmed.contains(":") then
              hasTypeContinuation = true
        
        parseParameter(content) match
          case Some(p) => params = params :+ p
          case None => ()
      }
      
      // Skip blocks with type continuations (too complex to handle)
      if params.length >= 2 && !hasTypeContinuation then
        blocks = blocks :+ ParamBlock(i, j - 1, baseIndent, openBracket, closeBracket, params, trailingContent)
      
      i = j
    else
      i += 1
  
  blocks

def processFile(path: os.Path): Boolean =
  val content = os.read(path)
  val lines = content.split("\n", -1).toIndexedSeq
  
  val blocks = findParamBlocks(lines)
  if blocks.isEmpty then return false
  
  var newLines = lines.toArray
  var changed = false
  
  // Process blocks in reverse order
  blocks.reverse.foreach { block =>
    var formatted = formatParameters(block.params, block.baseIndent, block.openBracket, block.closeBracket)
    
    // Add trailing content to last line
    if block.trailingContent.nonEmpty && formatted.nonEmpty then
      val lastLine = formatted.last
      val closeBracketStr = s" ${block.closeBracket}"
      if lastLine.endsWith(closeBracketStr) then
        formatted = formatted.init :+ (lastLine + block.trailingContent)
    
    val originalLines = (block.startLine to block.endLine).map(lines(_)).toList
    val origNorm = originalLines.map(_.stripTrailing())
    val newNorm = formatted.map(_.stripTrailing())
    
    if newNorm != origNorm && formatted.nonEmpty then
      val before = newLines.take(block.startLine)
      val after = newLines.drop(block.endLine + 1)
      newLines = before ++ formatted ++ after
      changed = true
  }
  
  if changed then
    os.write.over(path, newLines.mkString("\n"))
    println(s"Aligned: $path")
  
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
  
  println(s"Aligned $count files")

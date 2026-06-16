                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package ypsiloid

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

// `y"…"` interpolator and extractor macros.
//
// Modelled on jacinta.internal — and structurally near-identical — but
// uses a *sentinel-string* hole marker instead of a null-byte parser
// hole mode. The macro joins parts with `MarkerString`, parses the
// result via `YamlParser`, and recognises occurrences of
// `Str(MarkerString)` in the resulting AST as placeholders. This
// avoids any parser change.
//
// Errors reported by `YamlParser` carry `(offset, length)` on the
// `ParseError.position` (set by `YamlParser.errorAt`); a custom
// `HaltTactic` translates those positions back through the
// part-origins map to source-file ranges, so diagnostics highlight
// the exact bad span in the user's `y"…"` template.
object internal:
  // A unique sentinel that is highly unlikely to appear in normal
  // YAML content. Any occurrence in the parsed AST is treated as a
  // hole produced by the macro's part-joining step.
  private final val MarkerString: String = "__YPHOLE__"

  private def hasMarker(s: String): Boolean = s.contains(MarkerString)

  private def stripPad(arr: IArray[Any]): IArray[Any] =
    val n = arr.length

    if n > 0 && (arr(n - 1).asInstanceOf[AnyRef] eq Yaml.Ast.arrayPad) then arr.take(n - 1)
    else arr

  private def preprocess(parts: List[String]): (List[String], Set[Int]) =
    var spreads: Set[Int] = Set()

    val cleaned: List[String] = parts.zipWithIndex.map: (part, idx) =>
      if idx > 0 && part.startsWith("*") then
        spreads = spreads + (idx - 1)
        part.substring(1).nn
      else
        part

    (cleaned, spreads)


  // Reuses `YamlPath`'s own `Decodable` for validation: the literal is decoded
  // at macro-expansion time and, if it fails, the `YamlPathError`'s offset is
  // mapped back to a source position so the error points exactly at the
  // offending character. Mirrors `jacinta.internal.jsonPointer`.
  def yamlPath[parts <: Tuple: Type, origins <: Tuple: Type](insertions: Expr[Seq[Any]])
  :   Macro[YamlPath] =

    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    def firstOrigin[tuple: Type]: Int = Type.of[tuple] match
      case '[head *: tail] => TypeRepr.of[head].dealias match
        case AppliedType(_, ConstantType(IntConstant(start)) :: _) => start
        case _                                                     => 0

      case _ => 0

    val parts = recur[parts](Nil)
    if parts.length != 1 then halt(m"a YAML path literal cannot have substitutions")
    val raw: String = parts.head
    val start: Int = firstOrigin[origins]

    try unsafely(raw.tt.decode[YamlPath]) catch
      case error: YamlPathError =>
        val sourceFile = Position.ofMacroExpansion.sourceFile

        val position = sourceFile.content match
          case Some(content: String) if start > 0 && start < content.length =>
            val upper = (start + raw.length*6 + 16).min(content.length)
            val mapping = Interpolation.buildMapping(content.substring(start, upper).nn, raw)
            val at = (start + mapping(error.offset.min(raw.length))).min(content.length - 1)
            Position(sourceFile, at, (at + 1).min(content.length))

          case _ =>
            Position.ofMacroExpansion

        halt(error.message, position)

    '{unsafely(${Expr(raw)}.tt.decode[YamlPath])}

  def interpolator[parts <: Tuple: Type, origins <: Tuple: Type]
    ( insertions0: Expr[Seq[Any]] )
  :   Macro[Yaml] =

    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    def recurOrigins[tuple: Type](acc: List[(Int, Int)]): List[(Int, Int)] =
      Type.of[tuple] match
        case '[head *: tail] =>
          val pair = TypeRepr.of[head].dealias match
            case AppliedType(_, List(ConstantType(IntConstant(s)), ConstantType(IntConstant(e)))) =>
              (s, e)

            case _ =>
              (0, 0)

          recurOrigins[tail](pair :: acc)

        case _ =>
          acc.reverse

    val partOrigins: List[(Int, Int)] = recurOrigins[origins](Nil)

    val insertions: Seq[Expr[Any]] = insertions0.absolve match
      case Varargs(insertions) => insertions

    val (parts2, spreads) = preprocess(parts)
    val source: String = parts2.mkString(MarkerString)

    val sourceFile = Position.ofMacroExpansion.sourceFile
    val macroPos = Position.ofMacroExpansion

    val sourceContent: Optional[String] = sourceFile.content match
      case Some(s: String) => s
      case _               => Unset

    val perPart: IndexedSeq[((String, Int, Int), Int => Int)] =
      parts.zip(parts2).zip(partOrigins).map: pair =>
        val ((origPart, parserPart), (srcStart, _)) = pair
        val srcSkip = origPart.length - parserPart.length
        val effectiveStart = srcStart + srcSkip

        val mapping: Int => Int = sourceContent.lay(identity[Int]): content =>
          if effectiveStart > 0 && effectiveStart < content.length then
            val upper = (effectiveStart + parserPart.length * 6 + 16).min(content.length)
            val sourceText = content.substring(effectiveStart, upper).nn
            Interpolation.buildMapping(sourceText, parserPart)
          else
            (i: Int) => i

        ((parserPart, effectiveStart, srcSkip), mapping)

      . toIndexedSeq

    def translateOffset(parserOff: Int, len: Int): Position =
      var acc = 0
      var i = 0

      while i < perPart.length do
        val ((parserPart, effectiveStart, _), mapping) = perPart(i)
        val partLen = parserPart.length

        if parserOff < acc + partLen && effectiveStart > 0 then
          val inPart = parserOff - acc
          val endIn = (inPart + len.max(1)).min(parserPart.length)
          val rawStart = (effectiveStart + mapping(inPart)).max(effectiveStart)
          val rawEnd = (effectiveStart + mapping(endIn)).max(rawStart + 1)
          return Position(sourceFile, rawStart, rawEnd)

        acc += partLen + MarkerString.length
        i += 1

      macroPos

    val ast: Yaml.Ast =
      given diagnostics: Diagnostics = Diagnostics.omit

      given parseTactic: HaltTactic[ParseError, Yaml.Ast] =
        new HaltTactic[ParseError, Yaml.Ast]:
          override def abort(error: Diagnostics ?=> ParseError): Nothing =
            val pe = error
            val off = pe.position.offset.or(0)
            val length = pe.position.length.or(0)
            halt(pe.labelled, translateOffset(off, length))

      YamlParser.parse(source.tt)

    abortive:

      var holeIndex: Int = 0

      def consumeHole(): Expr[Any] =
        val expr = insertions(holeIndex)
        holeIndex += 1
        expr

      def encodeValue(expr: Expr[Any]): Expr[Yaml.Ast] = expr.absolve match
        case '{$value: tpe} =>
          Expr.summon[(? >: tpe) is Encodable in Yaml] match
            case Some('{$enc: Encodable}) =>
              '{Yaml.unseal($enc.encode($value))}

            case _ =>
              halt
                ( m"a value of ${TypeRepr.of[tpe].show} is not Encodable in Yaml",
                  expr.asTerm.underlyingArgument.pos )

      def encodeText(expr: Expr[Any]): Expr[String] = expr.absolve match
        case '{$value: tpe} =>
          Expr.summon[(? >: tpe) is Encodable in Text] match
            case Some('{$enc: Encodable}) =>
              '{$enc.encode($value).s}

            case _ =>
              halt
                ( m"a value of ${TypeRepr.of[tpe].show} is not Encodable in Text",
                  expr.asTerm.underlyingArgument.pos )

      def encodeArraySpread(expr: Expr[Any]): Expr[Iterable[Yaml.Ast]] = expr.absolve match
        case '{$value: tpe} => Type.of[tpe] match
          case '[Iterable[t]] =>
            Expr.summon[(? >: t) is Encodable in Yaml] match
              case Some('{$enc: Encodable}) =>
                ' {
                    $value.asInstanceOf[Iterable[t]].iterator
                    . map: item => Yaml.unseal($enc.encode(item))
                    . to(Iterable)
                  }

              case _ =>
                halt
                  ( m"the elements of ${TypeRepr.of[tpe].show} are not Encodable in Yaml",
                    expr.asTerm.underlyingArgument.pos )

          case _ =>
            halt
              ( m"a `*`-spread requires an Iterable, but got ${TypeRepr.of[tpe].show}",
                expr.asTerm.underlyingArgument.pos )

      def encodeObjectRest(expr: Expr[Any]): Expr[Iterable[(String, Yaml.Ast)]] =
        expr.absolve match
          case '{$value: tpe} => Type.of[tpe] match
            case '[Map[Text, Yaml]] =>
              ' {
                  $value.asInstanceOf[Map[Text, Yaml]].iterator.map: (key, yaml) =>
                    (key.s, Yaml.unseal(yaml))

                  . toList
                }

            case _ =>
              halt
                ( m"""
                    an object rest hole requires a Map[Text, Yaml], but got
                    ${TypeRepr.of[tpe].show}
                  """,
                  expr.asTerm.underlyingArgument.pos )

      def serializeString(s: String): Expr[Yaml.Ast] =
        if !hasMarker(s) then '{Yaml.Ast(${Expr(s)})}
        else
          val parts: Array[String | Null] = s.split(MarkerString, -1).nn
          var resultExpr: Expr[String] = Expr(parts(0).nn)
          var i = 1

          while i < parts.length do
            val fragment = encodeText(consumeHole())
            val partExpr = Expr(parts(i).nn)
            resultExpr = '{$resultExpr + $fragment + $partExpr}
            i += 1

          '{Yaml.Ast(${resultExpr})}

      def serializeArray(elements: IArray[Any]): Expr[Yaml.Ast] =
        val n = elements.length

        val pieces: List[Expr[Iterable[Yaml.Ast]]] = elements.zipWithIndex.toList.map:
          (elem, idx) =>
            elem.asMatchable match
              case s: String if s == MarkerString =>
                if spreads.has(holeIndex) then
                  if idx != n - 1 then halt:
                    m"a `*`-spread is only allowed as the last element of a sequence"

                  encodeArraySpread(consumeHole())
                else
                  val v = encodeValue(consumeHole())
                  '{Iterable($v)}

              case other =>
                val v = serialize(other)
                '{Iterable($v)}

        ' {
            val all = ${Expr.ofList(pieces)}.foldLeft(List.empty[Yaml.Ast])(_ ++ _)
            Yaml.Ast.Sequence(IArray.from(all))
          }

      def serializeObject(node: IArray[Any]): Expr[Yaml.Ast] =
        val n = node.length/2

        val pieces: List[Expr[Iterable[(String, Yaml.Ast)]]] =
          (0 until n).toList.map: i =>
            val k = node(i*2).asInstanceOf[String]
            val v = node(i*2 + 1)

            if k == MarkerString then
              v.asMatchable match
                case s: String if s == MarkerString =>
                  encodeObjectRest(consumeHole())

                case _ => halt:
                  m"unexpected non-rest hole in mapping key position"
            else
              v.asMatchable match
                case s: String if s == MarkerString =>
                  val expr = encodeValue(consumeHole())
                  '{Iterable((${Expr(k)}, $expr))}

                case other =>
                  val expr = serialize(other)
                  '{Iterable((${Expr(k)}, $expr))}

        ' {
            val all =
              ${Expr.ofList(pieces)}.foldLeft(List.empty[(String, Yaml.Ast)])(_ ++ _)

            val arr = new Array[Any](all.length*2)
            var k = 0

            all.foreach: pair =>
              arr(k*2)     = Yaml.Ast.Str(pair(0).tt).asInstanceOf[Any]
              arr(k*2 + 1) = pair(1).asInstanceOf[Any]
              k += 1

            Yaml.Ast.mapFromAnyArray(arr)
          }

      def serialize(node: Any): Expr[Yaml.Ast] = node.asMatchable match
        case s: String if s == MarkerString =>
          if spreads.has(holeIndex) then halt:
            m"a `*`-spread is only allowed as the last element of a sequence"

          encodeValue(consumeHole())

        case s: String =>
          serializeString(s)

        case b: Boolean =>
          '{Yaml.Ast(${Expr(b)})}

        case l: Long =>
          '{Yaml.Ast(${Expr(l)})}

        case d: Double =>
          '{Yaml.Ast(${Expr(d)})}

        case null =>
          '{Yaml.Ast.Null}

        case arr: IArray[Any] @unchecked =>
          if (arr.length & 1) == 0 then serializeObject(arr)
          else serializeArray(stripPad(arr))

        case other =>
          halt(m"unexpected YAML AST node ${other.toString.tt}")

      val ofAst: Expr[Yaml.Ast] = serialize(ast)

      '{Yaml.ast($ofAst)}


  def extractor[parts <: Tuple: Type, origins <: Tuple: Type]
    ( scrutinee: Expr[Yaml] )
  :   Macro[Extrapolation[Yaml]] =

    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    abortive:
      val (parts2, spreads) = preprocess(parts)
      val source: String = parts2.mkString(MarkerString)

      val ast: Yaml.Ast =
        given diagnostics: Diagnostics = Diagnostics.omit
        YamlParser.parse(source.tt)

      var nextHole: Int = 0
      var types: List[TypeRepr] = Nil

      def descend
        ( array: Expr[Array[Any]],
         pattern: Any,
         scrutinee: Expr[Yaml.Ast],
         accept: Expr[Boolean] )
      :   Expr[Boolean] =

        pattern.asMatchable match
          case s: String if s == MarkerString =>
            val idx = nextHole

            if spreads.has(idx) then halt:
              m"a `*`-spread is only allowed as the last element of a sequence"

            nextHole += 1
            types ::= TypeRepr.of[Yaml]
            '{$accept && {$array(${Expr(idx)}) = Yaml.ast($scrutinee); true}}

          case s: String if hasMarker(s) =>
            halt:
              m"""
                holes inside string literals are not supported in extractors; capture
                the entire string as a single hole instead
              """

          case s: String =>
            ' {
                $accept && $scrutinee.isString
                && $scrutinee.asInstanceOf[String] == ${Expr(s)}
              }

          case b: Boolean =>
            ' {
                $accept && $scrutinee.isBoolean
                && $scrutinee.asInstanceOf[Boolean] == ${Expr(b)}
              }

          case l: Long =>
            ' {
                $accept && $scrutinee.isLong
                && $scrutinee.asInstanceOf[Long] == ${Expr(l)}
              }

          case d: Double =>
            ' {
                $accept && $scrutinee.isDouble
                && $scrutinee.asInstanceOf[Double] == ${Expr(d)}
              }

          case null =>
            '{$accept && $scrutinee.isNull}

          case arr: IArray[Any] @unchecked =>
            if (arr.length & 1) == 0 then descendObject(array, arr, scrutinee, accept)
            else descendArray(array, stripPad(arr), scrutinee, accept)

          case other =>
            halt(m"unexpected YAML AST node ${other.toString.tt}")

      def descendArray
        ( array: Expr[Array[Any]],
         elements: IArray[Any],
         scrutinee: Expr[Yaml.Ast],
         accept: Expr[Boolean] )
      :   Expr[Boolean] =

        val n = elements.length

        val tailSpread: Boolean =
          n > 0 && (elements(n - 1).asMatchable match
            case s: String if s == MarkerString =>
              spreads.has(nextHole + countHolesInPrefix(elements, n - 1))

            case _ => false)

        val prefixLen = if tailSpread then n - 1 else n

        val lengthCheck: Expr[Boolean] =
          if tailSpread then
            '{$accept && $scrutinee.isArray && $scrutinee.arrayLength >= ${Expr(prefixLen)}}
          else
            '{$accept && $scrutinee.isArray && $scrutinee.arrayLength == ${Expr(prefixLen)}}

        var combined: Expr[Boolean] = lengthCheck
        var i = 0

        while i < prefixLen do
          val el = elements(i)
          val itemExpr = '{$scrutinee.arrayElement(${Expr(i)})}
          combined = descend(array, el, itemExpr, combined)
          i += 1

        if tailSpread then
          val idx = nextHole
          nextHole += 1
          types ::= TypeRepr.of[Yaml]

          combined =
            ' {
                $combined && {
                  val total = $scrutinee.arrayLength
                  val tailLen = total - ${Expr(prefixLen)}
                  val tail = new Array[Any](tailLen)
                  var k = 0

                  while k < tailLen do
                    tail(k) = $scrutinee.arrayElement(${Expr(prefixLen)} + k).asInstanceOf[Any]
                    k += 1

                  $array(${Expr(idx)}) = Yaml.ast(Yaml.Ast.seqFromAnyArray(tail))
                  true
                }
              }

        combined

      def countHolesInPrefix(elements: IArray[Any], upTo: Int): Int =
        var count = 0
        var i = 0

        while i < upTo do
          count += countHolesIn(elements(i))
          i += 1

        count

      def countHolesIn(node: Any): Int = node.asMatchable match
        case s: String if s == MarkerString => 1

        case s: String =>
          var c = 0
          var idx = 0
          val mlen = MarkerString.length

          while idx + mlen <= s.length do
            if s.regionMatches(idx, MarkerString, 0, mlen) then
              c += 1
              idx += mlen
            else
              idx += 1

          c

        case arr: IArray[Any] @unchecked =>
          if (arr.length & 1) == 0 then
            val pairs = arr.length/2
            var c = 0
            var k = 0

            while k < pairs do
              if arr(k*2) == MarkerString then c += 1
              else c += countHolesIn(arr(k*2 + 1))

              k += 1

            c
          else
            val elems = stripPad(arr)
            var c = 0
            var k = 0

            while k < elems.length do
              c += countHolesIn(elems(k))
              k += 1

            c

        case _ => 0

      def descendObject
        ( array: Expr[Array[Any]],
         node: IArray[Any],
         scrutinee: Expr[Yaml.Ast],
         accept: Expr[Boolean] )
      :   Expr[Boolean] =

        val pairs = node.length/2

        val literalKeys: List[String] =
          (0 until pairs).toList.collect:
            case i if node(i*2).asInstanceOf[String] != MarkerString =>
              node(i*2).asInstanceOf[String]

        val hasRest: Boolean =
          (0 until pairs).exists: i => node(i*2).asInstanceOf[String] == MarkerString

        val cardinality: Expr[Boolean] =
          if hasRest then
            ' {
                $accept && $scrutinee.isObject
                && {
                  val n = $scrutinee.objectSize
                  var keysSet = Set.empty[String]
                  var k = 0

                  while k < n do
                    keysSet += $scrutinee.objectKey(k)
                    k += 1

                  ${Expr(literalKeys)}.forall(keysSet.contains)
                }
              }
          else
            ' {
                $accept && $scrutinee.isObject
                && {
                  val n = $scrutinee.objectSize

                  n == ${Expr(literalKeys.length)} && {
                    var keysSet = Set.empty[String]
                    var k = 0

                    while k < n do
                      keysSet += $scrutinee.objectKey(k)
                      k += 1

                    ${Expr(literalKeys)}.forall(keysSet.contains)
                  }
                }
              }

        var combined: Expr[Boolean] = cardinality
        var i = 0

        while i < pairs do
          val k = node(i*2).asInstanceOf[String]
          val v = node(i*2 + 1)

          if k == MarkerString then
            val idx = nextHole
            nextHole += 1
            types ::= TypeRepr.of[Yaml]
            val literalKeysExpr = Expr(literalKeys)

            combined =
              ' {
                  $combined && {
                    val n = $scrutinee.objectSize
                    val keep = $literalKeysExpr.toSet
                    val keysBuf = scala.collection.mutable.ArrayBuffer.empty[String]
                    val valsBuf = scala.collection.mutable.ArrayBuffer.empty[Any]
                    var j = 0

                    while j < n do
                      val key = $scrutinee.objectKey(j)

                      if !keep.has(key) then
                        keysBuf += key
                        valsBuf += $scrutinee.objectValue(j)

                      j += 1

                    val arr = new Array[Any](keysBuf.length*2)
                    var m = 0

                    while m < keysBuf.length do
                      arr(m*2)     = Yaml.Ast.Str(keysBuf(m).tt).asInstanceOf[Any]
                      arr(m*2 + 1) = valsBuf(m)
                      m += 1

                    $array(${Expr(idx)}) = Yaml.ast(Yaml.Ast.mapFromAnyArray(arr))
                    true
                  }
                }
          else
            val keyLiteral = Expr(k)

            val valueExpr =
              ' {
                  val idx2 = $scrutinee.objectIndexOf($keyLiteral)
                  if idx2 < 0 then null.asInstanceOf[Yaml.Ast] else $scrutinee.objectValue(idx2)
                }

            combined = descend(array, v, valueExpr, combined)

          i += 1

        combined

      val numberOfHoles =
        var c = 0
        var k = 0

        while k < parts2.length - 1 do
          c += 1
          k += 1

        c

      val result: Expr[Extrapolation[Yaml]] =
        ' {
            val extracts = new Array[Any](${Expr(numberOfHoles)})

            val matches: Boolean =
              ${descend('extracts, ast, '{Yaml.unseal($scrutinee)}, '{true})}

            $ {
                if numberOfHoles == 0 then '{matches}
                else if numberOfHoles == 1 then
                  '{if !matches then None else Some(extracts(0).asInstanceOf[Yaml])}
                else
                  '{if !matches then None else Some(Tuple.fromArray(extracts))}
              }
          }

      types.length match
        case 0 =>
          '{$result.asInstanceOf[Boolean]}

        case 1 =>
          types.head.asType.absolve match
            case '[type result <: Yaml; result] =>
              '{$result.asInstanceOf[Option[result]]}

        case _ =>
          AppliedType(defn.TupleClass(types.length).info.typeSymbol.typeRef, types.reverse)
          . asType
          . absolve match
            case '[type result <: Tuple; result] =>
              '{$result.asInstanceOf[Option[result]]}

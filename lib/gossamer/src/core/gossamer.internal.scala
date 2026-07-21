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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package gossamer

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import gigantism.*
import hieroglyph.*
import kaleidoscope.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object internal:
  // Both `t""` and `txt""` build a Text by escape-processing each static part
  // at compile time and converting each substitution via Showable at runtime.
  // Only the final treatment differs: txt collapses runs of whitespace and
  // double-newlines into single newlines for multi-line literals.
  private def textInterpolator
    ( context:    Expr[StringContext],
     insertions: Expr[Seq[Any]],
     normalize:  Boolean )
    ( using Quotes )
  :   Expr[Text] =

    import quotes.reflect.*

    val rawParts: List[String] =
      context.value.getOrElse:
        halt(m"the StringContext extension method parameter does not appear to be inline")

      . parts.toList

    val escapedParts: List[String] = rawParts.map: part =>
      try TextEscapes.escape(part.tt).s catch case error: EscapeError => error match
        case EscapeError(msg) => halt(msg)

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.toList

    val showedInsertions: List[Expr[String]] = insertionExprs.map: expr =>
      expr.absolve match
        case '{$value: tpe} =>
          Expr.summon[(? >: tpe) is Showable] match
            case Some('{$showable: Showable}) =>
              '{$showable.text($value).s}

            case _ =>
              halt(m"a value of ${TypeRepr.of[tpe].show} is not Showable")

    var concatExpr: Expr[String] = Expr(escapedParts.head)
    var i = 0

    while i < showedInsertions.length do
      val insertion = showedInsertions(i)
      val nextPart = Expr(escapedParts(i + 1))
      concatExpr = '{$concatExpr + $insertion + $nextPart}
      i += 1

    if normalize then
      ' {
          val array =
            $concatExpr.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn)

          anticipation.Text(String.join("\n", array*).nn)
        }
    else
      '{anticipation.Text($concatExpr)}


  def t(context: Expr[StringContext], insertions: Expr[Seq[Any]]): Macro[Text] =
    textInterpolator(context, insertions, normalize = false)


  def txt(context: Expr[StringContext], insertions: Expr[Seq[Any]]): Macro[Text] =
    textInterpolator(context, insertions, normalize = true)

  object opaques:
    opaque type Ascii = anticipation.Data
    opaque type Grapheme = String

    object Grapheme:
      def apply(string: String): Grapheme = string

      given showable: Grapheme is Showable = grapheme => grapheme.tt

      // Width of a grapheme is the maximum width of its constituent codepoints. For
      // combining-mark sequences (e.g. e + ́) this gives the base character's width;
      // for joiner-glued graphemes (RI flag pairs, ZWJ family emoji, Hangul jamo
      // syllables) this avoids the over-counting that summing per-codepoint widths
      // would produce — one cell on the terminal regardless of how many codepoints
      // make up the cluster.
      given measurable: (charM: Char is Measurable) => Grapheme is Measurable = grapheme =>
        val s: String = grapheme
        var max = 0
        var i = 0

        while i < s.length do
          val cp = Character.codePointAt(s, i)

          val w =
            if Character.charCount(cp) == 2
            then charM.width(s.charAt(i)) + charM.width(s.charAt(i + 1))
            else charM.width(s.charAt(i))

          if w > max then max = w
          i += Character.charCount(cp)

        max

      extension (grapheme: Grapheme)
        def text: Text = grapheme.tt
        def chars: Int = grapheme.length

    object Ascii:
      def apply(bytes: Data): Ascii = bytes

      given showable: Ascii is Showable =
        ascii => String(ascii.mutable(using Unsafe), "ASCII").nn.tt

      given concatenable: Ascii is Concatenable:
        type Operand = Ascii
        def concat(left: Ascii, right: Ascii): Ascii = textual.concat(left, right)

      extension (ascii: Ascii) def bytes: Data = ascii

      given textual: Ascii is Textual:
        type Result = Byte
        type Show[value] = value is Showable

        val empty: Ascii = IArray.from[Byte](Nil)
        val classTag: ClassTag[Ascii] = summon[ClassTag[Ascii]]

        def apply(text: Text): Ascii = text.sysData
        def single(operand: Byte): Ascii = IArray(operand)
        def fromChar(char: Char): Byte = char.toByte
        def length(ascii: Ascii): Int = ascii.size
        def text(ascii: Ascii): Text = String(ascii.mutable(using Unsafe), "ASCII").nn.tt
        def access(ascii: Ascii, index: Ordinal): Byte = ascii(index.n0)
        def builder(size: Optional[Int]): Builder[Ascii] = AsciiBuilder(size)
        def size(ascii: Ascii): Int = ascii.length

        def map(ascii: Ascii)(lambda: Byte => Byte): Ascii = ascii.map(lambda)

        def concat(left: Ascii, right: Ascii): Ascii =
          IArray.build[Byte](left.length + right.length): array =>
            array.place(left, Prim)
            array.place(right, left.length.z)

        def indexOf(ascii: Ascii, sub: Text, start: Ordinal): Optional[Ordinal] =
          ascii.indexOfSlice(apply(sub)).puncture(-1).let(_.z)

        def show[value](value: value)(using show: Show[value]): Ascii =
          Ascii(show.text(value).sysData)

        def segment(ascii: Ascii, interval: Interval): Ascii =
          ascii.slice(interval.start.n0, interval.limit.n0)

  def ascii(context: Expr[StringContext], parts: Expr[Seq[Ascii]]): Macro[Ascii] =
    val dynamicParts: List[Expr[Ascii]] = parts.absolve match
      case Varargs(parts) => parts.to(List)

    val staticParts: List[Expr[Ascii]] = context.value.get.parts.to(List).map: part =>
      val bytes: IArray[Expr[Byte]] = part.tt.chars.map: char =>
        if char >= 128 then halt(824, m"$char is not a valid ASCII character")
        Expr[Byte](char.toByte)

      '{Ascii(Data(${Varargs(bytes)}*))}

    def recur(first: List[Expr[Ascii]], second: List[Expr[Ascii]], expr: Expr[Ascii]): Expr[Ascii] =
      first match
        case head :: tail => recur(second, tail, '{$expr+$head})
        case Nil          => expr

    recur(staticParts.tail.to(List), dynamicParts, staticParts.head)


  def extractMacro[textual: Type, value: Type]
    ( text:    Expr[textual],
      start:   Expr[Ordinal],
      lambda:  Expr[Scanner ?=> textual ~> value],
      textual: Expr[textual is Textual] )
    ( using Quotes )
  :   Expr[LazyList[value]] =

    import quotes.reflect.*

    def unwrap(term: Term): Term = term match
      case Inlined(_, _, inner) => unwrap(inner)
      case other                => other

    val outer = unwrap(lambda.asTerm)

    val decomposed: Option[(Symbol, List[CaseDef])] = outer match
      case Block(List(DefDef(_, List(TermParamClause(List(scannerVal))), _, Some(body))), _) =>
        val scannerSym = scannerVal.symbol

        unwrap(body) match
          case Block(List(DefDef(_, _, _, Some(Match(_, caseDefs)))), _) =>
            Some((scannerSym, caseDefs))

          case _ =>
            None

      case _ =>
        None

    decomposed match
      case None =>
        // Cannot decompose — fall back to a runtime driver that advances by matchEnd.
        ' {
            val input = $textual.text($text)

            def step(from: Int): LazyList[value] =
              if from >= input.s.length then LazyList() else
                val scanner = Scanner(from)

                $lambda(using scanner).lift($text) match
                  case Some(head) =>
                    head #:: step(scanner.matchEnd.or(input.s.length).max(from + 1))

                  case _ =>
                    LazyList()

            step($start.n0)
          }

      case Some((scannerSym, caseDefs)) =>
        // Build one (Int, textual) => Option[(matchStart, matchEnd, value)] closure per case,
        // substituting the outer scanner param with a fresh local in each.
        val closures: List[Expr[(Int, textual) => Option[(Int, Int, value)]]] =
          caseDefs.map: caseDef =>
            '{ (from: Int, input: textual) =>
                val scanner = Scanner(from)

                $ {
                    val scannerRef = 'scanner.asTerm
                    val inputRef = 'input.asTerm

                    object Subst extends TreeMap:
                      override def transformTerm(tree: Term)(owner: Symbol): Term =
                        tree match
                          case Ident(_) if tree.symbol == scannerSym =>
                            scannerRef

                          case _ =>
                            super.transformTerm(tree)(owner)

                    val substituted = Subst.transformCaseDef(caseDef)(Symbol.spliceOwner)

                    val rhsWrapped =
                      ' {
                          Some
                            ( ( scanner.nextStart.or(Int.MaxValue),
                                scanner.matchEnd.or(Int.MaxValue),
                                ${substituted.rhs.asExprOf[value]} ) )
                        }

                      . asTerm

                    val matchedCase = CaseDef(substituted.pattern, substituted.guard, rhsWrapped)
                    val wildcard = CaseDef(Wildcard(), None, '{None}.asTerm)
                    val matchTerm = Match(inputRef, List(matchedCase, wildcard))
                    matchTerm.asExprOf[Option[(Int, Int, value)]]
                  }
              }

        ' {
            val input = $textual.text($text)
            val length = input.s.length
            val cases = ${Expr.ofList(closures)}

            def step(from: Int): LazyList[value] =
              if from >= length then LazyList() else
                var best: Optional[(Int, Int, value)] = Unset
                val it = cases.iterator

                while it.hasNext do
                  it.next()(from, $text) match
                    case Some(candidate) =>
                      best.let: existing =>
                        if candidate(0) < existing(0) then best = candidate

                      . or:
                        best = candidate

                    case _ =>

                best.lay(LazyList()): triple =>
                  triple(2) #:: step(triple(1).max(from + 1))

            step($start.n0)
          }


  def fuzzyMacro[result: Type]
    ( text:      Expr[Text],
      threshold: Expr[Double],
      cases:     Expr[Text ~> result],
      proximity: Expr[Proximity { type Operand = Double }] )
    ( using Quotes )
  :   Expr[result] =

    import quotes.reflect.*

    def unwrap(term: Term): Term = term match
      case Inlined(_, _, inner) => unwrap(inner)
      case other                => other

    val caseDefs: List[CaseDef] = unwrap(cases.asTerm) match
      case Block(List(DefDef(_, _, _, Some(Match(_, cs)))), _) =>
        cs

      case _ =>
        halt(m"fuzzy requires a partial-function literal")

    def isWildcard(pattern: Tree): Boolean = pattern match
      case Wildcard()          => true
      case Bind(_, Wildcard()) => true
      case _                   => false

    // Find the first String literal anywhere in the pattern tree.
    // Works for both `case "foo"` (Literal(StringConstant)) and `case t"foo"`
    // (Unapply on a SimpleTExtractor built from the literal).
    def findLiteral(tree: Tree): Option[String] =
      var found: Option[String] = None

      val finder = new TreeAccumulator[Unit]:
        def foldTree(unit: Unit, t: Tree)(owner: Symbol): Unit =
          if found.isDefined then () else t match
            case Literal(StringConstant(s)) => found = Some(s)
            case _                          => foldOverTree(unit, t)(owner)

      finder.foldTree((), tree)(Symbol.spliceOwner)
      found

    val (nonWildcard, wildcard) =
      if caseDefs.nonEmpty && isWildcard(caseDefs.last.pattern)
      then (caseDefs.init, Some(caseDefs.last))
      else (caseDefs, None)

    nonWildcard.foreach: cd =>
      if isWildcard(cd.pattern)
      then halt(m"the wildcard case in a fuzzy match must be the last case")

    val patternExprs: List[Expr[Text]] = nonWildcard.map: cd =>
      findLiteral(cd.pattern) match
        case Some(s) => '{Text(${Expr(s)})}
        case None    => halt(m"fuzzy case patterns must be a string literal or `t\"…\"` literal")

    val nonWildcardThunks = nonWildcard.map: cd =>
      '{() => ${cd.rhs.asExprOf[result]}}

    val wildcardThunk = wildcard.map: cd =>
      val matchTerm = Match(text.asTerm, List(cd))
      '{() => ${matchTerm.asExprOf[result]}}

    val patternsExpr = Expr.ofList(patternExprs)
    val thunksExpr = Expr.ofList(nonWildcardThunks)

    wildcardThunk match
      case Some(wcThunk) =>
        ' {
            val scrutinee = $text
            val limit = $threshold
            val measure = $proximity
            val patterns = $patternsExpr
            val thunks = $thunksExpr
            var bestIdx = -1
            var bestDist = Double.PositiveInfinity
            var i = 0

            while i < patterns.length && bestDist > 0.0 do
              val d = measure.distance(scrutinee, patterns(i))

              if d <= limit && d < bestDist then
                bestDist = d
                bestIdx = i

              i += 1

            if bestIdx >= 0 then thunks(bestIdx)() else $wcThunk()
          }

      case None =>
        ' {
            val scrutinee = $text
            val limit = $threshold
            val measure = $proximity
            val patterns = $patternsExpr
            val thunks = $thunksExpr
            var bestIdx = -1
            var bestDist = Double.PositiveInfinity
            var i = 0

            while i < patterns.length && bestDist > 0.0 do
              val d = measure.distance(scrutinee, patterns(i))

              if d <= limit && d < bestDist then
                bestDist = d
                bestIdx = i

              i += 1

            if bestIdx >= 0 then thunks(bestIdx)() else throw new MatchError(scrutinee)
          }

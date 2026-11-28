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
package urticose

import scala.collection.immutable.List
import scala.collection.immutable.Nil
import scala.collection.immutable.Seq
import scala.compiletime.asMatchable
import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import spectacular.*
import symbolism.*
import vacuous.*

object protointernal:
  case class UrlInterpolatorError(detail: Message)
  extends Exception(s"urticose: ${detail.text.s}")

  // What the code of a `url"…"` literal calls for a substitution. The literal's own text is
  // parsed as the code compiles, and its parts are constructed directly; only a value which
  // is substituted has to be turned into the text of the part it stands in, and, where that
  // part is a host or a port, read as one.
  object Runtime:
    import unsafeExceptions.canThrowAny

    def text(fragment: Url.Fragment): Text = fragment match
      case Url.Fragment.Integral(value)   => value.show
      case Url.Fragment.Textual(text)     => text.urlEncode
      case Url.Fragment.RawTextual(text)  => text

    def host(text: Text): Host =
      try throwErrors[Hostname.Error](text.as[Host]) catch
        case error: Hostname.Error => throw UrlInterpolatorError(error.message)

    def ipv6(text: Text): Host =
      try throwErrors[IpAddress.Error](text.as[Ipv6]) catch
        case error: IpAddress.Error => throw UrlInterpolatorError(error.message)

    def port(fragment: Url.Fragment): Int = fragment match
      case Url.Fragment.Integral(value) => checked(value)
      case other                        => port(text(other))

    def port(text: Text): Int =
      try checked(text.s.toInt) catch case error: NumberFormatException =>
        throw UrlInterpolatorError(m"the port $text is not a number")

    private def checked(port: Int): Int =
      if port >= 0 && port <= 65535 then port
      else throw UrlInterpolatorError(m"the port $port is not in the range 0-65535")

  // A part of a URL's text, as the literal gives it: its own characters, or a substitution.
  private enum Piece:
    case Literal(text: String)
    case Hole(index: Int)

  def refined(context: Expr[StringContext], insertions: Expr[Seq[Any]])(using Quotes)
  :   Expr[Url[Label]] =

    import quotes.reflect.*
    import unsafeExceptions.canThrowAny

    val parts: List[String] =
      context.value.getOrElse:
        halt(m"the StringContext extension method parameter does not appear to be inline")

      . parts.toList

    val origins: List[(Int, Int)] =
      Interpolation.literalOrigins(context, parts.length).stdlib.toList

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.toList

    // Each substitution as the `Url.Fragment` its `Insertion` makes of it, and the text which
    // stands for it while the literal is checked: a substitution declares that text as its
    // label (`x` for text, `80` for a number), and any other insertion is a digit.
    type Hole = (Expr[Url.Fragment], String, Optional[Expr[Url.Fragment]])

    val holes0: List[Hole] = insertionExprs.map: insertion =>
      insertion.absolve match
        case '{$value: tpe} =>
          val typeclass: Expr[Insertion[Url.Fragment, tpe]] =
            Expr.summon[Insertion[Url.Fragment, tpe]].getOrElse:
              halt
                ( m"can't substitute ${TypeRepr.of[tpe].show} into a url-interpolator",
                  insertion.asTerm.pos )

          val label: String = typeclass.absolve match
            case '{$_ : Substitution[Url.Fragment, tpe, sub]} => TypeRepr.of[sub] match
              case ConstantType(StringConstant(label)) => label

              case _ =>
                halt(m"expected a literal string label for the substitution")

            case _ =>
              "1"

          // A number is a port. Anywhere else in a URL it may stand only if its type is declared
          // to be embeddable in one, by an `Embeddable` instance which says what it becomes
          // there. The type is widened first: the value's own is the singleton type of what
          // was written.
          val textual: Optional[Expr[Url.Fragment]] =
            if label != "80" then Unset else
              TypeRepr.of[tpe].widen.asType.absolve match
                case '[wide] =>
                  Expr.summon[wide is Embeddable by Url.Fragment].map: embeddable =>
                    '{$embeddable.embed(${value.asExprOf[wide]})}: Expr[Url.Fragment]

                  . getOrElse(Unset)

          ('{$typeclass.embed($value)}, label, textual)

    // The text which is checked, and where in it each part of the literal and each substitution
    // lies: `starts(i)` is where part `i` begins, and its substitution follows it.
    val builder = new java.lang.StringBuilder()

    val starts: List[Int] = parts.zipWithIndex.map: (part, index) =>
      val start = builder.length
      builder.append(part)
      if index < holes0.length then builder.append(holes0(index)(1))
      start

    val check: String = builder.toString.nn

    def holeStart(index: Int): Int = starts(index) + parts(index).length
    def holeEnd(index: Int): Int = holeStart(index) + holes0(index)(1).length

    // The place in the source of an offset into the checked text: within a part of the literal,
    // its character; within a substitution, the expression substituted.
    def position(offset0: Int): Position =
      val offset = offset0.max(0).min((check.length - 1).max(0))
      val index = starts.lastIndexWhere(_ <= offset).max(0)
      val within = offset - starts(index)

      if within < parts(index).length || index >= holes0.length then
        Interpolation.sourcePosition
          ( proscenium.List(parts(index)),
            proscenium.List(origins(index)),
            0,
            within.min((parts(index).length - 1).max(0)) )
      else
        insertionExprs(index).asTerm.pos

    val url: Url[Label] =
      try throwErrors[Url.Error](check.tt.as[Url[Label]]) catch case error: Url.Error =>
        halt
          ( 100, 8, error.reason, m"the URL is not valid: ${error.reason}",
            position(error.offset.n0) )

    // The parts of the text, found as `Url`'s own decoder finds them.
    val colon = check.indexOf(':')
    val authoritative = check.startsWith("//", colon + 1)

    def first(from: Int, to: Int)(predicate: Char => Boolean): Int =
      var index = from
      while index < to && !predicate(check.charAt(index)) do index += 1
      if index < to then index else -1

    def orElse(index: Int, default: Int): Int = if index < 0 then default else index

    val authorityStart = colon + 3

    val pathStart =
      if !authoritative then colon + 1 else
        orElse
          ( first(authorityStart, check.length)(char => char == '/' || char == '?' || char == '#'),
            check.length )

    val mark = first(pathStart, check.length)(_ == '?')
    val hash = first(if mark < 0 then pathStart else mark + 1, check.length)(_ == '#')
    val pathEnd = if mark >= 0 then mark else orElse(hash, check.length)

    holes0.indices.foreach: index =>
      if holeStart(index) <= colon then
        halt(m"the scheme of a URL cannot be a substitution", insertionExprs(index).asTerm.pos)

    // Where the port lies, if the URL has one: a number may be substituted there, and nowhere
    // else unless it can be written as text.
    val portRange: (Int, Int) =
      if !authoritative then (-1, -1) else
        val arobase = first(authorityStart, pathStart)(_ == '@')
        val hostStart = if arobase < 0 then authorityStart else arobase + 1

        if hostStart < pathStart && check.charAt(hostStart) == '[' then
          val close = first(hostStart, pathStart)(_ == ']')
          if close + 1 < pathStart then (close + 2, pathStart) else (-1, -1)
        else
          val portColon = first(hostStart, pathStart)(_ == ':')
          if portColon < 0 then (-1, -1) else (portColon + 1, pathStart)

    val holes: List[(Expr[Url.Fragment], String)] = holes0.indices.toList.map: index =>
      val (fragment, label, textual) = holes0(index)
      val inPort = holeStart(index) >= portRange(0) && holeEnd(index) <= portRange(1)

      if label != "80" || inPort then (fragment, label) else
        val text = textual.or:
          halt
            ( m"""
                a number can be substituted into a URL only as its port, after a colon; elsewhere
                it can be shown as text, or its type needs an `Embeddable by Url.Fragment`
                instance
              """,
              insertionExprs(index).asTerm.pos )

        (text, label)

    def pieces(from: Int, to: Int): List[Piece] =
      val inside = holes0.indices.toList.filter: index =>
        holeStart(index) >= from && holeEnd(index) <= to

      val (last, found) = inside.foldLeft((from, List[Piece]())):
        case ((start, pieces), index) =>
          val before =
            if holeStart(index) > start
            then List(Piece.Literal(check.substring(start, holeStart(index)).nn)) else Nil

          (holeEnd(index), pieces ++ before :+ Piece.Hole(index))

      if last < to then found :+ Piece.Literal(check.substring(last, to).nn) else found

    def literal(pieces: List[Piece]): Boolean = pieces.forall:
      case Piece.Literal(_) => true
      case Piece.Hole(_)    => false

    def text(pieces: List[Piece]): Expr[Text] =
      if literal(pieces) then
        val joined = pieces.collect { case Piece.Literal(text) => text }.mkString
        '{${Expr(joined)}.tt}
      else
        val string = pieces.foldLeft('{""}): (string, piece) =>
          piece match
            case Piece.Literal(text) => '{$string+${Expr(text)}}
            case Piece.Hole(index)   => '{$string+Runtime.text(${holes(index)(0)}).s}

        '{$string.tt}

    def optional(from: Int, to: Int): Expr[Optional[Text]] = text(pieces(from, to))

    val authority: Expr[Optional[Authority]] =
      if !authoritative then '{Unset} else
        val parsed: Authority = url.authority.assume
        val arobase = first(authorityStart, pathStart)(_ == '@')
        val hostStart = if arobase < 0 then authorityStart else arobase + 1

        val userInfo: Expr[Optional[Text]] =
          if arobase < 0 then '{Unset} else optional(authorityStart, arobase)

        // A host in brackets is an IPv6 address, which may be followed by a port; any other host
        // ends at the first colon, as it does for the decoder.
        val bracketed = hostStart < pathStart && check.charAt(hostStart) == '['

        val (hostFrom, hostTo, portStart) =
          if bracketed then
            val close = first(hostStart, pathStart)(_ == ']')
            (hostStart + 1, close, if close + 1 < pathStart then close + 2 else -1)
          else
            val portColon = first(hostStart, pathStart)(_ == ':')
            (hostStart, orElse(portColon, pathStart), if portColon < 0 then -1 else portColon + 1)

        val hostPieces = pieces(hostFrom, hostTo)

        val host: Expr[Host] =
          if !literal(hostPieces) then
            if bracketed then '{Runtime.ipv6(${text(hostPieces)})}
            else '{Runtime.host(${text(hostPieces)})}
          else (parsed.host: Any).asMatchable.absolve match
            case hostname: Hostname => Expr(hostname)
            case ipv6: Ipv6         => Expr(ipv6)
            case ipv4: Int          => '{Ipv4(${Expr(ipv4)})}

        val port: Expr[Optional[Int]] =
          if portStart < 0 then '{Unset} else pieces(portStart, pathStart) match
            case List(Piece.Hole(index)) => '{Runtime.port(${holes(index)(0)})}
            case portPieces if literal(portPieces) => Expr(parsed.port.assume)
            case portPieces => '{Runtime.port(${text(portPieces)})}

        // Named, so that the substitutions are evaluated in the order they are written.
        '{Authority(userInfo = $userInfo, host = $host, port = $port)}

    val location: Expr[Text] = text(pieces(pathStart, pathEnd))

    val query: Expr[Optional[Text]] =
      if mark < 0 then '{Unset} else optional(mark + 1, orElse(hash, check.length))

    val fragment: Expr[Optional[Text]] =
      if hash < 0 then '{Unset} else optional(hash + 1, check.length)

    val scheme: String = check.substring(0, colon).nn

    ConstantType(StringConstant(scheme)).asType.absolve match
      case '[type label <: Label; label] =>
        ' {
            new Url[label]
              ( Origin[label](Scheme[label](${Expr(scheme)}.tt), $authority),
                $location,
                $query,
                $fragment )
          }

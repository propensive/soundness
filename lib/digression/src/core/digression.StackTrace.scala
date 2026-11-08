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
package digression

import anticipation.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object StackTrace:
  case class Method(className: Text, method: Text):
    private lazy val pivot = className.s.lastIndexOf(".")

    lazy val cls: Text = if pivot >= 0 then className.s.substring(pivot + 1).nn.tt else className.s
    lazy val prefix: Text = if pivot >= 0 then className.s.substring(0, pivot).nn.tt else "".tt

  object Frame:
    // What a frame turned out to be, once resolved against the definitions the compiler recorded.
    // Everything from `Bridge` onwards is plumbing the compiler synthesized, which renderers may
    // reasonably dim or drop.
    enum Kind:
      case Method, Lambda, Value, Class, Constructor, Extension, Default
      case Bridge, Forwarder, Initializer, Specialized, Synthetic

      def plumbing: Boolean = this match
        case Bridge | Forwarder | Initializer | Specialized | Synthetic => true
        case _                                                          => false

    // The source-level definition a frame was compiled from. `owner` is the chain of enclosing
    // definitions and `name` the definition's own source name, so that renderers which show the
    // class and the method in separate columns can keep doing so. `path` is the full source path,
    // of which a stack trace records only the last segment.
    case class Source
      ( path: Text, owner: Text, name: Text, kind: Kind, code: Optional[Text] = Unset ):

      def definition: Text = if owner.s.isEmpty then name else (owner.s+"."+name.s).tt

    // Where a frame's code was written, when the classfile's SMAP (the JSR-45
    // `SourceDebugExtension` attribute) records that it was inlined from another source file: one
    // entry per level of inlining, innermost first. `path` is the full source path, of which
    // `file` is the last segment. When a resolver could also find the definition the position
    // falls within—the inline method itself—`source` names it, just as `Frame.source` names the
    // frame's own definition.
    case class Inlined(file: Text, path: Text, line: Int, source: Optional[Source] = Unset)

  case class Frame
    ( method:    Method,
      file:      Text,
      line:      Optional[Int],
      native:    Boolean,
      jvmClass:  Text = "".tt,
      jvmMethod: Text = "".tt,
      source:    Optional[Frame.Source] = Unset,
      inlined:   List[Frame.Inlined] = Nil ):

    // What to show for the frame's owner: the chain of source definitions it sits inside, when
    // that is known, and otherwise the demangled class name.
    def displayClass: Text = source.lay(method.className)(_.owner)

    // What to show for the frame itself: its source name, when that is known, and otherwise the
    // demangled method name.
    def displayMethod: Text = source.lay(method.method)(_.name)

    private def pivot: Int = displayClass.s.lastIndexOf(".")

    // `displayClass` without its last segment.
    def displayPrefix: Text =
      if pivot >= 0 then displayClass.s.substring(0, pivot).nn.tt else "".tt

    // The last segment of `displayClass`.
    def displaySegment: Text =
      if pivot >= 0 then displayClass.s.substring(pivot + 1).nn.tt else displayClass

  object Resolver:
    given none: Resolver = frame => frame

  // Recovers the source-level definition behind each frame's compiled name. Resolution needs
  // capabilities — at minimum a classpath — which this module deliberately does not have, so the
  // default does nothing, and an implementation is supplied from elsewhere by importing it.
  trait Resolver:
    def resolve(frame: Frame): Frame

  // In `StackTrace`'s own companion rather than `Showable`'s, so that `spectacular` need not
  // depend on `digression`; being companion-to-companion, the implicit scope is unchanged.
  given showable: StackTrace is Showable = stack =>
    val methodWidth = stack.frames.map(_.displayMethod.s.length).maximum.or(0)
    val classWidth = stack.frames.map(_.displayClass.s.length).maximum.or(0)
    val fileWidth = stack.frames.map(_.file.s.length).maximum.or(0)
    val fullClass = s"${stack.component}.${stack.className}".tt
    val init = s"$fullClass: ${stack.message}".tt

    val root = stack.frames.fold(init):
      case (msg, frame) =>
        val obj = frame.method.className.s.endsWith("#")
        val drop = if frame.source.absent && obj then 1 else 0
        val file = " ".repeat(fileWidth - frame.file.s.length).nn+frame.file
        val dot = if frame.source.present || obj then ".".tt else "#".tt
        val className = frame.displayClass.s.dropRight(drop)
        val classPad = " ".repeat(classWidth - className.length).nn.tt
        val method = frame.displayMethod
        val methodPad = " ".repeat(methodWidth - method.s.length).nn.tt
        val line = frame.line.let(_.show).or("?".tt)
        val code = frame.source.let(_.code).lay("".tt): code => s"\n       $code".tt

        // Each level of inlining the SMAP recorded, innermost first: extra detail about the same
        // frame, so it is indented beneath it like a quoted line of source. When the position
        // resolved to a definition, the inline method is named ahead of its position.
        val inlined = frame.inlined.fold(""):
          case (text, origin) =>
            val where = origin.source.lay(s"${origin.file}:${origin.line}"): source =>
              s"${source.definition} (${origin.file}:${origin.line})"

            s"$text\n       ↳ inlined from $where"

        s"$msg\n  at $classPad$className$dot$method$methodPad $file:$line$code$inlined".tt

    stack.cause.lay(root): cause => s"$root\ncaused by:\n$cause".tt

  // A name the compiler mints after the program's own names are fixed, as `NameKinds` and
  // `StdNames` in the Scala compiler define them, and the glyph it is shown as. The table is the
  // one description of the encoding: `rewrite` reads it, `legend` is derived from it, and
  // hyperbole's `StackResolver.hint` classifies a frame by it. `kind` is what a method name
  // carrying the marker is known to be, for the markers which settle that.
  case class Marker(jvm: Text, glyph: Text, meaning: Text, kind: Optional[Frame.Kind] = Unset)

  object Marker:
    val adapted: Marker = Marker("$adapted".tt, "Λ".tt, "adapted lambda".tt, Frame.Kind.Bridge)
    val lambda: Marker = Marker("$anonfun".tt, "λ".tt, "lambda".tt, Frame.Kind.Lambda)
    val anonymous: Marker = Marker("$anon".tt, "α".tt, "anonymous class".tt)
    val constructor: Marker =
      Marker("<init>".tt, "ⲛ".tt, "constructor".tt, Frame.Kind.Constructor)


    val staticInitializer: Marker =
      Marker("<clinit>".tt, "ι".tt, "initializer".tt, Frame.Kind.Initializer)

    val traitInitializer: Marker =
      Marker("$init$".tt, "ι".tt, "initializer".tt, Frame.Kind.Initializer)

    val lazyInitializer: Marker =
      Marker("$lzyINIT".tt, "ℓ".tt, "lazy value".tt, Frame.Kind.Initializer)

    val lazyHandle: Marker = Marker("$lzyHandle".tt, "ℓ".tt, "lazy value".tt)
    val lazyField: Marker = Marker("$lzy".tt, "ℓ".tt, "lazy value".tt)

    val superAccessor: Marker =
      Marker("super$".tt, "↑".tt, "super accessor".tt, Frame.Kind.Forwarder)

    val protectedAccessor: Marker =
      Marker("protected$".tt, "⇢".tt, "accessor".tt, Frame.Kind.Forwarder)

    val inlineAccessor: Marker = Marker("inline$".tt, "⇢".tt, "accessor".tt, Frame.Kind.Forwarder)

    val parameterAccessor: Marker =
      Marker("$accessor".tt, "⇢".tt, "accessor".tt, Frame.Kind.Forwarder)

    val setter: Marker = Marker("$_setter_$".tt, "≔".tt, "trait setter".tt, Frame.Kind.Synthetic)
    val extension: Marker =
      Marker("$extension".tt, "⊢".tt, "extension method".tt, Frame.Kind.Extension)

    val direct: Marker = Marker("$direct".tt, "∂".tt, "direct method".tt)
    val default: Marker = Marker("$default".tt, "δ".tt, "default argument".tt, Frame.Kind.Default)

    val retained: Marker =
      Marker("$retainedBody".tt, "⊡".tt, "retained inline body".tt, Frame.Kind.Synthetic)

    val outer: Marker = Marker("$outer".tt, "⊙".tt, "outer reference".tt)
    val packageFile: Marker = Marker("$package".tt, "⁆".tt, "package file".tt)

    val specialized: Marker =
      Marker("$mc".tt, "Σ".tt, "specialized method, with its signature".tt, Frame.Kind.Specialized)

    // The JVM's own class for a lambda, `Outer$$Lambda/0x…`, is not the compiler's work but is
    // shown as a lambda all the same.
    val hiddenLambda: Marker = Marker("$$Lambda".tt, "λ".tt, "lambda".tt)

    // A module class, `Foo$`, is shown as `ΞFoo`; the glyph is applied by `rewrite` after the
    // name is rewritten, so the marker here is only for the legend.
    val module: Marker = Marker("$".tt, "Ξ".tt, "object".tt)

    // In the order `rewrite` tries them, so a marker precedes any other it begins with, and
    // `hint` reads them, so `$adapted` is seen before the `$anonfun` it always follows.
    val all: List[Marker] =
      List
        ( adapted, lambda, anonymous, constructor, staticInitializer, traitInitializer,
          lazyInitializer, lazyHandle, lazyField, superAccessor, protectedAccessor,
          inlineAccessor, parameterAccessor, setter, extension, direct, default, retained,
          outer, packageFile, specialized, hiddenLambda, module )

    // The operator characters and their encodings, as `scala.reflect.NameTransformer` has them.
    val operators: List[(Text, Char)] =
      List
        ( ("$tilde".tt, '~'), ("$eq".tt, '='), ("$less".tt, '<'), ("$greater".tt, '>'),
          ("$bang".tt, '!'), ("$hash".tt, '#'), ("$percent".tt, '%'), ("$up".tt, '^'),
          ("$amp".tt, '&'), ("$bar".tt, '|'), ("$times".tt, '*'), ("$div".tt, '/'),
          ("$plus".tt, '+'), ("$minus".tt, '-'), ("$colon".tt, ':'), ("$bslash".tt, '\\'),
          ("$qmark".tt, '?'), ("$at".tt, '@') )

  // Each glyph `rewrite` produces, with what it means; a number following a glyph is the
  // compiler's own numbering, in subscript, and `⁅…⁆` brackets a package file's name.
  val legend: Map[Text, Text] =
    Marker.all.fold(Map[Text, Text]()): (map, marker) =>
      if map.at(marker.glyph).present then map else map.define(marker.glyph, marker.meaning)
    . define("⁅⁆".tt, "package file".tt)

  def rewrite(name: String, method: Boolean = false): Text =
    val buffer: StringBuilder = StringBuilder()
    val separator: String = if method then "." else "#"

    def char(index: Int): Char = name.charAt(index)

    def primitive(char: Char) = char match
      case 'Z' => "Boolean"
      case 'B' => "Byte"
      case 'S' => "Short"
      case 'C' => "Char"
      case 'I' => "Int"
      case 'J' => "Long"
      case 'F' => "Float"
      case 'D' => "Double"
      case 'V' => "Unit"
      case _   => "?"

    def subscript(char: Char): Char = (char - '0' + '₀').toChar

    def at(index: Int, string: Text): Boolean = name.startsWith(string.s, index)

    // A marker beginning with a letter starts a name, so matches only where a name can start.
    def marker(index: Int): Optional[Marker] =
      val startable = index == 0 || !Character.isLetterOrDigit(char(index - 1))

      Marker.all.seek: marker =>
        marker != Marker.module && at(index, marker.jvm)
        && (startable || marker.jvm.s.startsWith("$") || marker.jvm.s.startsWith("<"))

    // The signature a specialized name encodes: `$mcII$sp` is a method whose type arguments,
    // in the order the compiler sorts them, are `Int` and `Int`. For the function classes
    // which are all the compiler specializes, the first is the result and the rest the
    // parameters.
    def specialized(index: Int): Optional[(String, Int)] =
      val end = name.indexOf("$sp", index + 3)
      val tags = if end < 0 then "" else name.substring(index + 3, end).nn

      if end < 0 || tags.isEmpty || tags.exists(primitive(_) == "?") then Unset else
        val result = primitive(char(index + 3))
        val params = tags.drop(1).map(primitive(_))

        val rendered = params.length match
          case 0 => s"Σ(() -> $result)"
          case 1 => s"Σ(${params.head} -> $result)"
          case _ => s"Σ((${params.mkString(", ")}) -> $result)"

        (rendered, end + 3)

    // A `$uXXXX` escape, as `NameTransformer` encodes a character the JVM disallows.
    def unicode(index: Int): Optional[Char] =
      if name.length < index + 6 || char(index + 1) != 'u' then Unset else
        val hex = name.substring(index + 2, index + 6).nn
        if hex.forall { char => Character.digit(char, 16) >= 0 }
        then Integer.parseInt(hex, 16).toChar
        else Unset

    @tailrec
    def digits(index: Int): Int =
      if index < name.length && char(index).isDigit then
        buffer.append(subscript(char(index)))
        digits(index + 1)
      else index

    def glyph(marker: Marker): Unit = marker match
      case Marker.adapted =>
        // The bridge the compiler adapts a lambda's method with: the lambda it adapts is
        // marked rather than the name lengthened.
        val lambda = buffer.lastIndexOf("λ")
        if lambda < 0 then buffer.append("Λ") else buffer.setCharAt(lambda, 'Λ')

      case Marker.hiddenLambda => buffer.append(separator).append("λ")
      case other               => buffer.append(other.glyph.s)

    @tailrec
    def recur(index: Int): Unit = if index < name.length then
      marker(index) match
        case Marker.specialized => specialized(index) match
          case (rendered: String, next: Int) => buffer.append(rendered) yet recur(digits(next))
          case _                             => buffer.append(separator) yet recur(index + 1)

        case marker: Marker => glyph(marker) yet recur(digits(index + marker.jvm.s.length))

        case _ => char(index) match
          case '$' =>
            Marker.operators.seek { (code, _) => at(index, code) } match
              case (code: Text, operator: Char) =>
                buffer.append(operator) yet recur(index + code.s.length)

              case _ => unicode(index) match
                case char: Char => buffer.append(char) yet recur(index + 6)

                // The compiler's numbering, `$1`, is shown in subscript with no separator.
                case _ if index + 1 < name.length && char(index + 1).isDigit =>
                  recur(digits(index + 1))

                case _ =>
                  // An expanded name joins its owner and member with `$$`: one separator,
                  // unless a marker follows the second `$`.
                  val expanded = at(index + 1, "$".tt) && marker(index + 1).absent
                  buffer.append(separator)
                  recur(if expanded then index + 2 else index + 1)

          case other => buffer.append(other) yet recur(index + 1)

    recur(0)
    val rewritten: String = buffer.toString+(if method then "()" else "")

    // The JVM's specialized function interfaces read better as the function types they stand
    // for: `scala.runtime.java8.JFunction1$mcII$sp` is `(Int => Int)`.
    if name.startsWith("scala.runtime.java8.JFunction") && name.endsWith("$sp") then
      specialized(name.indexOf("$mc")) match
        case (rendered: String, _) =>
          rendered.drop(1).replace(" -> ", " => ").nn.tt

        case _ => rewritten.tt

    else if name.startsWith("scala.runtime.function.JProcedure") then
      val n = try name.drop(33).toInt catch case error: Exception => 0

      val args =
        if n < 2 then s"Any" else List.iterator(List.fill(n)("Any")).mkString("(", ", ", ")")

      "("+args+" => Unit)".tt

    else if rewritten.endsWith("#") then
      val pivot = rewritten.lastIndexOf(".")

      // With no `.` to split on (`pivot == -1`), `substring(0, pivot)` would throw, so leave the
      // name unchanged rather than out-of-bounds.
      if pivot < 0 then rewritten.tt
      else
        val sub = if rewritten.endsWith("⁆#") then "⁅" else "Ξ"
        val head = rewritten.substring(0, pivot).nn
        val tail = rewritten.substring(pivot + 1).nn.dropRight(1)

        (head+"."+sub+tail).tt
    else
      rewritten.tt

  def apply(exception: Throwable)(using resolver: Resolver): StackTrace =
    val frames = exception.getStackTrace.nn.iterator.map(_.nn).to(List).map: frame =>
      val jvmClass = frame.getClassName.nn
      val jvmMethod = frame.getMethodName.nn

      resolver.resolve:
        StackTrace.Frame(
          StackTrace.Method(rewrite(jvmClass), rewrite(jvmMethod, method = true)),
          Text(Option(frame.getFileName).map(_.nn).getOrElse("———")),
          if frame.getLineNumber < 0 then Unset else frame.getLineNumber,
          frame.isNativeMethod,
          jvmClass.tt,
          jvmMethod.tt
        )

    val cause = Option(exception.getCause)
    val fullClassName: Text = rewrite(exception.getClass.nn.getName.nn)

    val fullClass: List[Text] =
      (fullClassName.s.split("\\.").nn.iterator.map { part => Text(part.nn) }).to(List)

    val className: Text = List.last(fullClass)

    val component: Text =
      val length = fullClassName.s.length - className.s.length - 1
      Text(fullClassName.s.substring(0, 0.max(length)).nn)

    val message: Message = exception match
      case error: Error => error.message
      case other        => Message(Text(Option(exception.getMessage).map(_.nn).getOrElse("")))

    StackTrace(component, className, message, frames, cause.map(_.nn).map(StackTrace(_)).optional)

  given communicable: StackTrace is Communicable = stack =>
    val methodWidth = stack.frames.map(_.displayMethod.s.length).maximum.or(0)
    val classWidth = stack.frames.map(_.displayClass.s.length).maximum.or(0)
    val fileWidth = stack.frames.map(_.file.s.length).maximum.or(0)
    val fullClass = s"${stack.component}.${stack.className}".tt
    val init = s"$fullClass: ${stack.message}".tt
    val nbsp = "\u00a0".tt

    val root = stack.frames.fuse(init):
      val obj = next.method.className.s.endsWith("#")
      val drop = if next.source.absent && obj then 1 else 0
      val file = (nbsp*(fileWidth - next.file.s.length))+next.file
      val dot = if next.source.present || obj then ".".tt else "#".tt
      val className = next.displayClass.s.dropRight(drop)
      val classPad = (nbsp*(classWidth - className.length))
      val method = next.displayMethod
      val methodPad = (nbsp*(methodWidth - method.s.length))

      val line = next.line match
        case Unset => "?".tt
        case value => value.toString.tt

      val inlined = next.inlined.fold(""):
        case (text, origin) =>
          val where = origin.source.lay(s"${origin.file}:${origin.line}"): source =>
            s"${source.definition}$nbsp(${origin.file}:${origin.line})"

          s"$text\n${nbsp*7}↳$nbsp"+s"inlined$nbsp"+s"from$nbsp$where"

      s"$state\n\n${nbsp*2}at$nbsp$classPad$className$dot$method$methodPad$nbsp$file:$line$inlined"
      . tt

    Message:
      stack.cause.lay(root): cause => s"$root\ncaused by:\n\n$cause".tt

case class StackTrace
  ( component: Text,
    className: Text,
    message:   Message,
    frames:    List[StackTrace.Frame],
    cause:     Optional[StackTrace] ):

  def crop(cutClassName: Text, cutMethod: Text): StackTrace =
    val frames2 = frames.keep: f =>
      f.method.className != cutClassName || f.method.method != cutMethod

    StackTrace(component, className, message, frames2, cause)

  def drop(n: Int): StackTrace =
    StackTrace(component, className, message, frames.skip(n), cause)

  def dropRight(n: Int): StackTrace =
    StackTrace(component, className, message, frames.skip(n, Bidi.Rtl), cause)

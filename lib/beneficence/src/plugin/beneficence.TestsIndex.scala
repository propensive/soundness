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
package beneficence

import scala.collection.mutable

import dotty.tools.dotc.*, ast.tpd, core.*, Constants.*, Contexts.*, Flags.*, Names.*, Symbols.*,
    Types.*

// The static index of Probably tests: what `META-INF/probably/tests/<source>` records for each
// source file, so that a host — the fume test runner — can enumerate a classpath's tests, and
// arrange them into their hierarchy, WITHOUT running any suite.
//
// Nothing here is inferred from where a declaration sits in the source. Every declaration
// (`test`, `suite`, sedentary's `bench`, …) takes a contextual `probably.Testable`, and the
// typed tree says exactly which one the compiler chose:
//
//  - the `testable` member of a `Suite`: the declaration is at the root of that suite, which
//    is known by its id — its `Topic`, if the id is given, and otherwise derived from its
//    title as `probably.Suite.derive` derives it;
//  - the parameter of a `suite(…)` block, which the indexer has already placed: the declaration
//    is in that group;
//  - a parameter of a method (`def cases()(using Testable of "json")`): the declaration is
//    wherever that method is called from, which the `call` lines record, and its `Topic`
//    still names the suite, by its id;
//  - the parameter of an `impromptu` block, whose `Topic` is `Impromptu`: the declaration is
//    left out, and an `open` line marks the place as having more tests than are listed.
//
// The lines are tab-separated, with `\\`, `\t`, `\n` escaped in every field, `\/` for a `/`
// within a path segment, and `\*` standing for a part of a name which is only known at
// runtime (a substitution in `m"…"`, or a name that is not a literal at all):
//
//     suite  <class>  <topic>  <title>  <line>
//     group  <topic>  <method>  <path>  <name>  <moniker>  <line>
//     test   <topic>  <method>  <path>  <kind>  <name>  <moniker>  <tags>  <line>  <spread>
//     nest   <topic>  <method>  <path>  <class of the suite invoked>  <line>
//     call   <topic>  <method>  <path>  <method called>  <line>
//     open   <topic>  <method>  <path>  <line>
//
// `<topic>` is the suite's id (empty if the method is generic in it), `<method>` is empty for a
// declaration placed from a suite's root and otherwise the method whose parameter it hangs
// from, and `<path>` is the `/`-separated NAMES of the groups between that root and the
// declaration (a group's moniker is on its own `group` line, and a suite's title, which is
// the name its tests' ids are computed from, on its `suite` line). `<tags>` are comma-separated, and
// `<spread>` is `spread` for a test declared over axes, whose cells only a run enumerates.
//
// A test's id is not recorded, because it follows from the names: see `probably.Test.Id#id`.
object TestsIndex:
  val version: Int = 2

  // `probably.Suite.derive`: the id of a suite which is declared by its title alone.
  private def derive(title: String): String =
    val builder = new StringBuilder

    title.toLowerCase.nn.foreach: char =>
      if Character.isLetterOrDigit(char) then builder.append(char)
      else if builder.nonEmpty && builder.last != '-' then builder.append('-')

    if builder.nonEmpty && builder.last == '-' then builder.setLength(builder.length - 1)
    if builder.isEmpty then "suite" else builder.toString

  // What an id may be, so that it can be typed on a command line unquoted: a tag's rule.
  private val Admissible = "[A-Za-z_][A-Za-z0-9_-]*".r

  private case class Frame
    ( topic: Option[String], impromptu: Boolean, method: Option[String], path: List[String] ):

    def child(segment: String): Frame = copy(path = path :+ segment)

    def fields: List[String] =
      List(escape(topic.getOrElse("")), escape(method.getOrElse("")), path.map(segment).mkString("/"))

  private val hole: Char = '\u0000'

  private def escape(text: String): String =
    val builder = new StringBuilder
    text.foreach:
      case '\\'   => builder.append("\\\\")
      case '\t'   => builder.append("\\t")
      case '\n'   => builder.append("\\n")
      case `hole` => builder.append("\\*")
      case char   => builder.append(char)

    builder.toString

  private def segment(text: String): String = escape(text).replace("/", "\\/").nn

  // The text of a `Message` as `fulminate.Message#text` gives it: escapes decoded and backticks
  // dropped as the `m` macro does, then each line trimmed and joined to the last with a space.
  private def decode(part: String): String =
    val builder = new StringBuilder
    var index = 0

    while index < part.length do
      part.charAt(index) match
        case '\\' if index + 1 < part.length =>
          part.charAt(index + 1) match
            case 'u' if index + 6 <= part.length =>
              try builder.append(Integer.parseInt(part.substring(index + 2, index + 6), 16).toChar)
              catch case _: NumberFormatException => ()
              index += 6

            case char =>
              builder.append:
                char match
                  case 'n' => '\n'
                  case 'r' => '\r'
                  case 'f' => '\f'
                  case 'b' => '\b'
                  case 't' => '\t'
                  case 'e' => '\u001b'
                  case _   => char

              index += 2

        case '`' =>
          index += 1

        case char =>
          builder.append(char)
          index += 1

    builder.toString

  private def unwrap(text: String): String =
    val builder = new StringBuilder

    text.split("\n").nn.foreach: line =>
      val line2 = line.nn
      if line2.forall(_.isWhitespace) then builder.append("\n") else
        if builder.nonEmpty then builder.append(" ")
        builder.append(line2.replaceAll("^ *", "").nn.replaceAll(" *$", "").nn.replaceAll("\\s+", " ").nn)

    builder.toString

  // The content of an interpolated string literal with the given prefix (`m"…"`, `m"""…"""`),
  // its substitutions replaced by holes; `None` if the source is anything else.
  private def literal(prefix: String, source: String): Option[String] =
    val body: Option[String] =
      if source.startsWith(prefix + "\"\"\"") && source.endsWith("\"\"\"")
         && source.length >= prefix.length + 6
      then Some(source.substring(prefix.length + 3, source.length - 3).nn)
      else if source.startsWith(prefix + "\"") && source.endsWith("\"")
              && source.length >= prefix.length + 2
      then Some(source.substring(prefix.length + 1, source.length - 1).nn)
      else None

    body.map: body =>
      val builder = new StringBuilder
      var index = 0

      while index < body.length do
        body.charAt(index) match
          case '$' if index + 1 < body.length && body.charAt(index + 1) == '$' =>
            builder.append('$')
            index += 2

          case '$' if index + 1 < body.length && body.charAt(index + 1) == '{' =>
            var depth = 1
            index += 2

            while index < body.length && depth > 0 do
              if body.charAt(index) == '{' then depth += 1
              else if body.charAt(index) == '}' then depth -= 1
              index += 1

            builder.append(hole)

          case '$' if index + 1 < body.length && body.charAt(index + 1).isUnicodeIdentifierStart =>
            index += 1
            while index < body.length && body.charAt(index).isUnicodeIdentifierPart do index += 1
            builder.append(hole)

          case '\\' if index + 1 < body.length =>
            builder.append('\\').append(body.charAt(index + 1))
            index += 2

          case char =>
            builder.append(char)
            index += 1

      builder.toString

  private val Identifier = "n\"([^\"]*)\"".r

  // Indexes one compilation unit, returning its lines.
  def index(unit: CompilationUnit, testable: Symbol, suite: Symbol)(using Context): List[String] =
    val message: Symbol = getClassIfDefined("fulminate.Message")
    val codepoint: Symbol = getClassIfDefined("digression.Codepoint")
    val lines: mutable.Buffer[String] = mutable.Buffer()
    val frames: mutable.HashMap[Symbol, Frame] = mutable.HashMap()

    // The ids of the suite classes of this unit, given or derived.
    val suites: mutable.HashMap[Symbol, String] = mutable.HashMap()
    val content: Array[Char] = unit.source.content
    val topicName: TypeName = typeName("Topic")

    def source(tree: tpd.Tree): String =
      if tree.span.exists && tree.source == unit.source && tree.span.end <= content.length
      then new String(content, tree.span.start, tree.span.end - tree.span.start)
      else ""

    def line(tree: tpd.Tree): String =
      if tree.span.exists then (unit.source.offsetToLine(tree.span.start) + 1).toString else "0"

    def emit(fields: String*): Unit = lines += fields.mkString("\t")

    def strip(tree: tpd.Tree): tpd.Tree = tree match
      case tpd.Inlined(_, Nil, expansion) => strip(expansion)
      case tpd.Typed(expression, _)       => strip(expression)
      case tpd.NamedArg(_, argument)      => strip(argument)
      case tpd.Block(Nil, expression)     => strip(expression)
      case _                              => tree

    // The values of the local `val`s seen so far. An application with named or defaulted
    // arguments is typed as a block which first binds its earlier arguments to `val`s, so the
    // name of `bench(m"…")(target = …)` reaches the method as a reference to one of them.
    val bound: mutable.HashMap[Symbol, tpd.Tree] = mutable.HashMap()

    def bind(statements: List[tpd.Tree]): Unit = statements.foreach:
      case definition: tpd.ValDef if !definition.symbol.is(Mutable) && !definition.rhs.isEmpty =>
        bound(definition.symbol) = definition.rhs

      case _ =>
        ()

    def written(tree: tpd.Tree): tpd.Tree = strip(tree) match
      case identifier: tpd.Ident => bound.get(identifier.symbol).map(written).getOrElse(tree)
      case _                     => tree

    // The topic a `Testable`'s type gives it: a suite's literal name, or `Impromptu`.
    def topicOf(tpe: Type): (Option[String], Boolean) =
      val member = tpe.widen.member(topicName)

      if !member.exists then (None, false) else member.info match
        case TypeAlias(alias) =>
          val symbol = alias.typeSymbol

          if symbol.exists && symbol.name.toString == "Impromptu"
             && symbol.fullName.toString.startsWith("probably.")
          then (None, true)
          else alias.dealias match
            case ConstantType(Constant(name: String)) => (Some(name), false)
            case _                                    => (None, false)

        case _ =>
          (None, false)

    def path(symbol: Symbol): String =
      val name = symbol.fullName.toString.replace("$.", ".").nn
      if name.endsWith("$") then name.dropRight(1) else name

    def enclosingMethod(symbol: Symbol): Symbol =
      var owner = symbol
      while owner.exists && (!owner.is(Method) || owner.isAnonymousFunction) do owner = owner.owner
      owner

    // Where the declarations made with this contextual `Testable` belong.
    def resolve(argument: tpd.Tree): Frame =
      val symbol = strip(argument).symbol

      frames.getOrElse
        ( symbol,
          {
            val (topic, impromptu) = topicOf(argument.tpe)

            // A suite's own `Testable`: with a derived id, the topic does not say which suite,
            // but the member is selected from one.
            def owner: Option[String] = strip(argument) match
              case tpd.Select(qualifier, _) => suites.get(qualifier.tpe.widen.classSymbol)
              case _                        => None

            if symbol.exists && symbol.maybeOwner == suite && symbol.name.toString == "testable"
            then Frame(topic.orElse(owner), impromptu, None, Nil)
            else if symbol.exists && symbol.is(Param) && enclosingMethod(symbol).exists
            then Frame(topic, impromptu, Some(path(enclosingMethod(symbol))), Nil)
            else if symbol.exists then Frame(topic, impromptu, Some(path(symbol)), Nil)
            else Frame(topic, impromptu, Some("?"), Nil)
          } )

    def unroll(tree: tpd.Tree): (tpd.Tree, List[List[tpd.Tree]]) = tree match
      case tpd.Apply(function, arguments) =>
        val (core, lists) = unroll(function)
        (core, lists :+ arguments)

      case tpd.TypeApply(function, _) =>
        unroll(function)

      case _ =>
        (tree, Nil)

    def pair(method: Symbol, lists: List[List[tpd.Tree]]): List[(Symbol, tpd.Tree)] =
      val parameters = method.paramSymss.filter { list => list.isEmpty || list.head.isTerm }
      parameters.zip(lists).flatMap { (parameters, arguments) => parameters.zip(arguments) }

    // The parameters and arguments of an application and of whatever it is selected from, so
    // that `bench(m"…", n"slow")(target = …).over(axis)(…)` yields its name and tags.
    def pairs(tree: tpd.Tree): List[(Symbol, tpd.Tree)] = strip(tree) match
      case tpd.Block(statements, expression) =>
        bind(statements)
        pairs(expression)

      case tree: (tpd.Apply | tpd.TypeApply) if tree.symbol.exists =>
        val (core, lists) = unroll(tree)

        pair(tree.symbol, lists) ++ (core match
          case tpd.Select(qualifier, _) => pairs(qualifier)
          case _                        => Nil)

      case _ =>
        Nil

    def isTestable(tpe: Type): Boolean = tpe.widen.derivesFrom(testable)

    def named(tpe: Type, name: String): Boolean =
      val symbol = tpe.typeSymbol
      symbol.exists && symbol.name.toString == name

    def nameOf(pairs: List[(Symbol, tpd.Tree)]): String =
      pairs.collectFirst:
        case (parameter, argument) if message.exists && parameter.info.derivesFrom(message) =>
          literal("m", source(written(argument))).map { text => unwrap(decode(text)) }
          . getOrElse(hole.toString)

      . getOrElse(hole.toString)

    def monikerOf(pairs: List[(Symbol, tpd.Tree)]): String =
      pairs.collectFirst:
        case (parameter, argument)
          if named(parameter.info, "Name")
             && parameter.info.argInfos.headOption.exists(named(_, "Probing")) =>

          Identifier.findFirstMatchIn(source(written(argument))).map(_.group(1).nn).getOrElse("")

      . getOrElse("")

    def tagsOf(pairs: List[(Symbol, tpd.Tree)]): String =
      pairs.collectFirst:
        case (parameter, argument)
          if parameter.info.isRepeatedParam
             && parameter.info.argInfos.headOption.exists(named(_, "Tag")) =>

          Identifier.findAllMatchIn(source(written(argument))).map(_.group(1).nn).mkString(",")

      . getOrElse("")

    def kindOf(method: Symbol): String =
      val owner = method.owner.fullName.toString
      if owner.startsWith("sedentary.Bench") then "bench"
      else if owner.startsWith("sedentary.Stress") then "stress"
      else if owner.startsWith("sedentary.Profile") then "profile"
      else "check"

    // The contextual `Testable` parameters of a block passed as an argument.
    def blockParameters(argument: tpd.Tree): List[Symbol] = strip(argument) match
      case tpd.Block((definition: tpd.DefDef) :: Nil, _: tpd.Closure) =>
        definition.termParamss.flatten.map(_.symbol).filter { symbol => isTestable(symbol.info) }

      case _ =>
        Nil

    val traverser = new tpd.TreeTraverser:
      // The frame of the enclosing suite, block or method, for what takes no `Testable`.
      private var lexical: Option[Frame] = None

      // Whether the declaration being visited is the receiver of `over`.
      private var spread: Boolean = false

      private def within(frame: Option[Frame])(action: => Unit): Unit =
        val saved = lexical
        lexical = frame
        try action finally lexical = saved

      def traverse(tree: tpd.Tree)(using Context): Unit = tree match
        case tree: tpd.TypeDef
          if tree.symbol.isClass && tree.symbol != suite && tree.symbol.derivesFrom(suite) =>

          val (stated, _) = topicOf(tree.symbol.thisType)

          // The title is the `Message` passed to `Suite`'s constructor.
          val title: String = tree.rhs match
            case template: tpd.Template =>
              template.parents.collectFirst:
                case parent: tpd.Apply if parent.symbol.maybeOwner == suite => nameOf(pairs(parent))

              . getOrElse(hole.toString)

            case _ =>
              hole.toString

          stated.foreach: id =>
            if !Admissible.matches(id) then
              report.error
                ( s"the suite id \"$id\" must be a letter or `_` followed by letters, digits, `_` "
                  + "or `-`, so that it can be typed on a command line",
                  tree.srcPos )

          val topic: Option[String] =
            stated.orElse(if title.contains(hole) then None else Some(derive(title)))

          topic.foreach { id => suites(tree.symbol) = id }

          if tree.symbol.is(Module) && tree.symbol.isStatic then
            emit
              ( "suite",
                escape(path(tree.symbol)),
                escape(topic.getOrElse("")),
                escape(title),
                line(tree) )

          within(Some(Frame(topic, false, None, Nil)))(traverseChildren(tree))

        case tree: tpd.DefDef if !tree.symbol.isAnonymousFunction =>
          val parameter =
            tree.termParamss.flatten.map(_.symbol).find: symbol =>
              symbol.is(Given) && isTestable(symbol.info)

          parameter match
            case Some(parameter) =>
              // A `Testable` with no topic satisfies no declaration, so one written in this
              // method would silently take whichever `Testable` the enclosing scope has.
              if !parameter.info.widen.member(topicName).info.isInstanceOf[TypeAlias] then
                report.error
                  ( "this Testable has no topic, so a test declared in this method would not be "
                    + "declared for it: write `Testable of \"<the suite's name>\"`, or `Testable of "
                    + "topic` for a type parameter `topic <: Label`",
                    parameter.srcPos )

              val (topic, impromptu) = topicOf(parameter.info)
              within(Some(Frame(topic, impromptu, Some(path(tree.symbol)), Nil))):
                traverseChildren(tree)

            case None =>
              traverseChildren(tree)

        case tree: tpd.Apply =>
          application(tree)

        case tree: tpd.Block =>
          bind(tree.stats)
          traverseChildren(tree)

        case _ =>
          traverseChildren(tree)

      private def application(tree: tpd.Apply)(using Context): Unit =
        val method = tree.symbol
        val (core, lists) = unroll(tree)
        val paired = if method.exists && method.is(Method) then pair(method, lists) else Nil

        val context: Option[Frame] =
          paired.collectFirst:
            case (parameter, argument) if parameter.is(Given) && isTestable(argument.tpe) =>
              resolve(argument)

        val block: Option[tpd.Tree] =
          paired.collectFirst:
            case (parameter, argument)
              if defn.isContextFunctionType(parameter.info)
                 && parameter.info.dealias.argInfos.headOption.exists(isTestable) =>

              argument

        val declares: Boolean =
          codepoint.exists && paired.exists: (parameter, _) =>
            parameter.is(Given) && parameter.info.derivesFrom(codepoint)

        // A block's frame is registered for its parameters before anything inside is visited.
        var inner: Option[Frame] = None

        (context, block) match
          case (Some(frame), Some(block)) =>
            val parameters = blockParameters(block)
            val impromptu = parameters.exists { symbol => topicOf(symbol.info)(1) }

            val frame2: Frame =
              if impromptu then
                if !frame.impromptu then emit(("open" :: frame.fields ::: List(line(tree)))*)
                frame.copy(impromptu = true)
              else
                val name = nameOf(paired)
                val moniker = monikerOf(paired)

                if !frame.impromptu then
                  emit(("group" :: frame.fields ::: List(escape(name), escape(moniker), line(tree)))*)

                frame.child(name)

            parameters.foreach { symbol => frames(symbol) = frame2 }
            inner = Some(frame2)

          case (Some(frame), None) if !frame.impromptu =>
            if declares then
              val all = paired ++ (core match
                case tpd.Select(qualifier, _) => pairs(qualifier)
                case _                        => Nil)

              val axial = spread || method.name.toString == "over"
              spread = false

              emit
                ( ("test" :: frame.fields ::: List
                    ( kindOf(method),
                      escape(nameOf(all)),
                      escape(monikerOf(all)),
                      escape(tagsOf(all)),
                      line(tree),
                      if axial then "spread" else "" ))* )
            else if !method.name.toString.contains("$default$") then
              emit(("call" :: frame.fields ::: List(escape(path(method)), line(tree)))*)

          case _ =>
            if method.exists && method.name.toString == "apply" && method.maybeOwner == suite then
              core match
                case tpd.Select(qualifier, _) =>
                  val invoked: String = path(qualifier.tpe.widen.classSymbol)

                  lexical.filter(!_.impromptu).foreach: frame =>
                    emit(("nest" :: frame.fields ::: List(escape(invoked), line(tree)))*)

                case _ =>
                  ()

        core match
          case tpd.Select(qualifier, _) =>
            val saved = spread
            spread = context.isEmpty && method.exists && method.name.toString == "over"
            try traverse(qualifier) finally spread = saved

          case _ =>
            ()

        spread = false

        lists.flatten.foreach: argument =>
          if block.exists(_ eq argument) then within(inner)(traverse(argument))
          else traverse(argument)

    traverser.traverse(unit.tpdTree)
    lines.toList

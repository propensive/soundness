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
package xenophile

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

import TypescriptError.Reason

// A parser for TypeScript declaration files.
//
// It is a *declaration* parser, not a TypeScript compiler: it reads the shapes a `.d.ts` file may
// contain and nothing about expressions or statements, which such a file does not have. Within
// that scope it is total in the only sense that matters — every construct is either understood or
// rejected. The previous grammar here skipped what it did not recognise, which quietly turned a
// generic or `extends`-bearing interface into no interface at all; for a foreign-function facade
// that produced a confusing "no such member" later, and for a compatibility discipline it would
// have produced an unsound claim.
//
// Constructs deliberately outside the grammar — conditional types, mapped types, template literal
// types, `infer`, decorators — raise `Unsupported` rather than being approximated.
object TypescriptParser:

  enum Token:
    case Word(text: Text)
    case Str(text: Text)
    case Num(text: Text)
    case Punct(text: Text)

    def show: Text = this match
      case Word(text)  => text
      case Str(text)   => t"'$text'"
      case Num(text)   => text
      case Punct(text) => text

  // Multi-character punctuation, longest first, so `=>` never lexes as `=` then `>`, and `...`
  // never as three dots.
  private val operators: scala.List[String] =
    scala.List("...", "=>", "?.", "??", "<=", ">=", "===", "!==", "==", "!=", "&&", "||")

  private val singles: String = "{}()[]<>:;,.?|&=+-*/!#@"

  def lex(source: Text): List[Token] raises TypescriptError =
    val text = source.s
    val tokens = scala.collection.mutable.ListBuffer[Token]()
    var index = 0

    def word(start: Int): Int =
      var end = start
      while end < text.length && (text.charAt(end).isLetterOrDigit || text.charAt(end) == '_'
          || text.charAt(end) == '$') do end += 1

      tokens += Token.Word(text.substring(start, end).nn.tt)
      end

    def number(start: Int): Int =
      var end = start
      while end < text.length && (text.charAt(end).isDigit || text.charAt(end) == '.'
          || text.charAt(end) == 'x' || text.charAt(end) == 'e'
          || (text.charAt(end) >= 'a' && text.charAt(end) <= 'f')
          || (text.charAt(end) >= 'A' && text.charAt(end) <= 'F')) do end += 1

      tokens += Token.Num(text.substring(start, end).nn.tt)
      end

    // A string literal's *value* is the token, so `"a"` and `'a'` — the same literal type written
    // two ways — lex identically and cannot differ in any encoding computed from these tokens.
    def string(start: Int, quote: Char): Int raises TypescriptError =
      val builder = StringBuilder()
      var end = start + 1

      while end < text.length && text.charAt(end) != quote do
        if text.charAt(end) == '\\' && end + 1 < text.length then
          builder.append(text.charAt(end + 1))
          end += 2
        else
          builder.append(text.charAt(end))
          end += 1

      if end >= text.length
      then abort(TypescriptError(Reason.Syntax(t"the string literal is unterminated", t"$quote")))

      tokens += Token.Str(builder.toString.tt)
      end + 1

    while index < text.length do
      val char = text.charAt(index)

      if char.isWhitespace then index += 1
      else if char == '/' && index + 1 < text.length && text.charAt(index + 1) == '/' then
        while index < text.length && text.charAt(index) != '\n' do index += 1
      else if char == '/' && index + 1 < text.length && text.charAt(index + 1) == '*' then
        index += 2
        while index + 1 < text.length
        && !(text.charAt(index) == '*' && text.charAt(index + 1) == '/') do index += 1
        index = (index + 2).min(text.length)
      else if char == '`' then
        abort(TypescriptError(Reason.Unsupported(t"a template literal type")))
      else if char == '"' || char == '\'' then index = string(index, char)
      else if char.isDigit then index = number(index)
      else if char.isLetter || char == '_' || char == '$' then index = word(index)
      else
        val operator = operators.find: operator =>
          text.regionMatches(index, operator, 0, operator.length)

        operator match
          case scala.Some(operator) =>
            tokens += Token.Punct(operator.tt)
            index += operator.length

          case scala.None =>
            if singles.indexOf(char.toInt) >= 0 then
              tokens += Token.Punct(char.toString.tt)
              index += 1
            else abort(TypescriptError(Reason.Syntax(t"unexpected character", char.toString.tt)))

    List.from(tokens.toList)

  def parse(source: Text): List[TypescriptDeclaration] raises TypescriptError =
    Cursor(lex(source)).declarations()

  // A mutable cursor over the token stream. Declaration grammars are almost entirely
  // single-token-lookahead, and threading an index through forty mutually recursive functions
  // obscures the grammar without making it any more correct.
  private class Cursor(tokens: List[Token]):
    private val items: scala.collection.immutable.Vector[Token] = tokens.stdlib.toVector

    // The cursor is a plain `Int` into an immutable vector, so it captures nothing; the
    // annotation says so, rather than making the whole parser a tracked capability.
    @scala.caps.unsafe.untrackedCaptures private var position: Int = 0

    def peek(ahead: Int = 0): Optional[Token] =
      if position + ahead < items.length then items(position + ahead) else Unset

    def next(): Optional[Token] =
      val token = peek()
      position += 1
      token

    def here: Text = peek().lay(t"the end of the file")(_.show)

    def word: Optional[Text] = peek() match
      case Token.Word(text) => text
      case _                => Unset

    def punct: Optional[Text] = peek() match
      case Token.Punct(text) => text
      case _                 => Unset

    def at(text: Text): Boolean = word == text || punct == text

    // Lookahead by token text, without constructing an `Optional` to compare against: the
    // comparison would search for a `CanEqual` over the `Unset | Token` union.
    def ahead(offset: Int, text: Text): Boolean = peek(offset) match
      case Token.Word(value)  => value == text
      case Token.Punct(value) => value == text
      case _                  => false

    def skip(text: Text): Boolean = if at(text) then { position += 1; true } else false

    def expect(text: Text): Unit raises TypescriptError =
      if !skip(text)
      then abort(TypescriptError(Reason.Syntax(t"expected $text", here)))

    def identifier(): Text raises TypescriptError = next() match
      case Token.Word(text) => text
      case Token.Str(text)  => text
      case Token.Num(text)  => text
      case _ => abort(TypescriptError(Reason.Syntax(t"expected a name", here)))

    // Consumes a `;` or `,` separator where the grammar permits either, and tolerates its
    // absence: a newline terminates a member in TypeScript, and the lexer has discarded newlines.
    def separator(): Unit =
      skip(t";")
      skip(t",")
      ()

    // --- declarations ------------------------------------------------------------------------

    // A file with any top-level `import` or `export` is a *module*, in which only exported
    // declarations are reachable by a consumer; otherwise it is a global script, in which every
    // top-level declaration is. The distinction is settled by scanning the whole token stream
    // once, because an `export` at the end of a file retroactively determines the status of a
    // declaration at the top.
    private def moduleForm: Boolean =
      var found = false
      var index = 0

      while !found && index < items.length do
        items(index) match
          case Token.Word(text) => found = text == t"export" || text == t"import"
          case _                => ()

        index += 1

      found

    def declarations(): List[TypescriptDeclaration] raises TypescriptError =
      val module = moduleForm
      val result = scala.collection.mutable.ListBuffer[TypescriptDeclaration]()
      block(Nil, module, result, ambient = false)

      val seen = scala.collection.mutable.HashSet[Text]()

      // Interfaces and namespaces merge in TypeScript; classes, aliases and enums do not. A
      // repeated non-mergeable name is a source error, not something to resolve by last-wins.
      def mergeable(declaration: TypescriptDeclaration): Boolean = declaration match
        case _: TypescriptDeclaration.Interface => true
        case _: TypescriptDeclaration.Function  => true
        case _                                  => false

      result.toList.foreach: declaration =>
        val key = declaration.key
        if !mergeable(declaration) && !seen.add(key)
        then abort(TypescriptError(Reason.Duplicate(key)))

      List.from(result.toList)

    // `ambient` marks a namespace body, where every declaration is exported implicitly: an
    // ambient namespace has no notion of a private member, so its contents are as reachable as
    // the namespace itself.
    private def block
      ( scope:   TypescriptDeclaration.Scope,
        module:  Boolean,
        into:    scala.collection.mutable.ListBuffer[TypescriptDeclaration],
        ambient: Boolean )
    :   Unit raises TypescriptError =

      while peek().present && !at(t"}") do
        if at(t"}") then () else declaration(scope, module, into, ambient)

      ()

    private def declaration
      ( scope:   TypescriptDeclaration.Scope,
        module:  Boolean,
        into:    scala.collection.mutable.ListBuffer[TypescriptDeclaration],
        ambient: Boolean )
    :   Unit raises TypescriptError =

      if skip(t";") then ()
      else if at(t"import") then skipStatement()
      else if at(t"@") then abort(TypescriptError(Reason.Unsupported(t"a decorator")))
      else
        val exported = skip(t"export")

        // `export default …`, `export = …` and `export { … }` re-export existing names; they
        // change nothing about the declarations themselves, which are parsed where they stand.
        if exported && (at(t"default") || at(t"=") || at(t"{") || at(t"*")) then skipStatement()
        else
          skip(t"declare")
          declared(scope, module, into, ambient, exported || ambient || !module)

    private def declared
      ( scope:   TypescriptDeclaration.Scope,
        module:  Boolean,
        into:    scala.collection.mutable.ListBuffer[TypescriptDeclaration],
        ambient: Boolean,
        visible: Boolean )
    :   Unit raises TypescriptError =

      if at(t"namespace") || at(t"module") || at(t"global") then
        val keyword = identifier()
        val name = if keyword == t"global" then t"global" else identifier()
        expect(t"{")
        // The namespace's own visibility becomes its contents': an unexported namespace exports
        // nothing, however its members are written.
        block(List.from(scope.stdlib :+ name), module, into, ambient = visible)
        expect(t"}")
      else if at(t"interface") then into += interfaceDeclaration(scope, visible)
      else if at(t"class") || at(t"abstract") then into += classDeclaration(scope, visible)
      else if at(t"type") then into += aliasDeclaration(scope, visible)
      else if at(t"enum") || at(t"const") && ahead(1, t"enum")
      then into += enumDeclaration(scope, visible)
      else if at(t"function") then into += functionDeclaration(scope, visible)
      else if at(t"const") || at(t"let") || at(t"var") then into += variableDeclaration(scope, visible)
      else abort(TypescriptError(Reason.Unsupported(t"a top-level ${here} declaration")))

    // `import` and re-export forms are recorded by their absence: they bind no new contract of
    // their own, so the parser advances past them to the statement terminator.
    private def skipStatement(): Unit =
      var depth = 0

      while peek().present && !(depth == 0 && (at(t";") || at(t"}"))) do
        if at(t"{") then depth += 1
        if at(t"}") then depth -= 1
        next()

      skip(t";")
      ()

    private def interfaceDeclaration(scope: TypescriptDeclaration.Scope, exported: Boolean)
    :   TypescriptDeclaration raises TypescriptError =

      expect(t"interface")
      val name = identifier()
      val typed = typeParameters()
      val extending = if skip(t"extends") then typeList() else Nil
      expect(t"{")
      val members = memberList()
      expect(t"}")

      TypescriptDeclaration.Interface(name, scope, typed, extending, members, exported)

    private def classDeclaration(scope: TypescriptDeclaration.Scope, exported: Boolean)
    :   TypescriptDeclaration raises TypescriptError =

      val isAbstract = skip(t"abstract")
      expect(t"class")
      val name = identifier()
      val typed = typeParameters()
      val extending: Optional[TypescriptType] = if skip(t"extends") then typeExpression() else Unset
      val implements = if skip(t"implements") then typeList() else Nil
      expect(t"{")
      val members = memberList()
      expect(t"}")

      TypescriptDeclaration.Class
        (name, scope, typed, extending, implements, members, isAbstract, exported)

    private def aliasDeclaration(scope: TypescriptDeclaration.Scope, exported: Boolean)
    :   TypescriptDeclaration raises TypescriptError =

      expect(t"type")
      val name = identifier()
      val typed = typeParameters()
      expect(t"=")
      val target = typeExpression()
      separator()

      TypescriptDeclaration.Alias(name, scope, typed, target, exported)

    private def enumDeclaration(scope: TypescriptDeclaration.Scope, exported: Boolean)
    :   TypescriptDeclaration raises TypescriptError =

      val constant = skip(t"const")
      expect(t"enum")
      val name = identifier()
      expect(t"{")
      val members = scala.collection.mutable.ListBuffer[(Text, Optional[Text])]()

      while !at(t"}") && peek().present do
        val member = identifier()
        val value: Optional[Text] = if skip(t"=") then identifier() else Unset
        members += ((member, value))
        separator()

      expect(t"}")

      TypescriptDeclaration.Enumeration(name, scope, List.from(members.toList), constant, exported)

    private def functionDeclaration(scope: TypescriptDeclaration.Scope, exported: Boolean)
    :   TypescriptDeclaration raises TypescriptError =

      expect(t"function")
      val name = identifier()
      val typed = typeParameters()
      val parameters = parameterList()
      val result = if skip(t":") then typeExpression() else TypescriptType.Named(t"void")
      separator()

      TypescriptDeclaration.Function
        (name, scope, List(TypescriptType.Function(parameters, result, typed)), exported)

    private def variableDeclaration(scope: TypescriptDeclaration.Scope, exported: Boolean)
    :   TypescriptDeclaration raises TypescriptError =

      val constant = at(t"const")
      next()
      val name = identifier()
      val typed: Optional[TypescriptType] = if skip(t":") then typeExpression() else Unset
      separator()

      TypescriptDeclaration.Variable(name, scope, typed, constant, exported)

    // --- members -----------------------------------------------------------------------------

    private def memberList(): List[TypescriptMember] raises TypescriptError =
      val members = scala.collection.mutable.ListBuffer[TypescriptMember]()

      while !at(t"}") && peek().present do
        member().let { value => members += value }
        separator()

      // Overload groups are folded here rather than in the grammar: consecutive signatures under
      // one name and kind are one member with several signatures, in declaration order, because
      // that order is what TypeScript's overload resolution reads.
      val merged = scala.collection.mutable.LinkedHashMap[Text, TypescriptMember]()

      members.toList.foreach: member =>
        val selector = member.selector

        merged.get(selector) match
          case scala.Some(existing) =>
            val signatures: List[TypescriptType] =
              List.from(existing.signatures.stdlib ++ member.signatures.stdlib)

            merged.put(selector, existing.copy(signatures = signatures))

          case scala.None => merged.put(selector, member)

      List.from(merged.values.toList)

    private def member(): Optional[TypescriptMember] raises TypescriptError =
      if skip(t";") then Unset
      else if at(t"@") then abort(TypescriptError(Reason.Unsupported(t"a decorator")))
      else declaredMember()

    private def declaredMember(): TypescriptMember raises TypescriptError =
      var visibility = TypescriptMember.Visibility.Public
      var static = false
      var readonly = false
      var isAbstract = false

      var scanning = true

      while scanning do
        if at(t"public") then { next(); visibility = TypescriptMember.Visibility.Public }
        else if at(t"protected") then { next(); visibility = TypescriptMember.Visibility.Protected }
        else if at(t"private") then { next(); visibility = TypescriptMember.Visibility.Private }
        else if at(t"static") then { next(); static = true }
        else if at(t"abstract") then { next(); isAbstract = true }
        // `readonly` is only a modifier when something follows it that can be named; `readonly`
        // as a member name is legal TypeScript.
        else if at(t"readonly") && !(ahead(1, t":")
            || ahead(1, t"?")
            || ahead(1, t"(")) then { next(); readonly = true }
        else scanning = false

      // A call signature `(…): T`, or a generic one `<T>(…): U`.
      if at(t"(") || at(t"<") then
        val typed = typeParameters()
        val parameters = parameterList()
        val result = if skip(t":") then typeExpression() else TypescriptType.Named(t"any")

        TypescriptMember
          ( t"", TypescriptMember.Kind.Call,
            List(TypescriptType.Function(parameters, result, typed)),
            visibility, static, readonly )

      // A construct signature `new (…): T`.
      else if at(t"new") && (ahead(1, t"(") || peek(1)
          == Optional(Token.Punct(t"<"))) then
        next()
        val typed = typeParameters()
        val parameters = parameterList()
        val result = if skip(t":") then typeExpression() else TypescriptType.Named(t"any")

        TypescriptMember
          ( t"", TypescriptMember.Kind.Construct,
            List(TypescriptType.Function(parameters, result, typed, construct = true)),
            visibility, static, readonly )

      // An index signature `[key: string]: T`. Distinguished from a computed property name
      // (`[Symbol.iterator]()`) by the `:` after the key.
      else if at(t"[") && ahead(2, t":") then
        expect(t"[")
        val key = identifier()
        expect(t":")
        val keyType = typeExpression()
        expect(t"]")
        expect(t":")
        val value = typeExpression()

        TypescriptMember
          ( key, TypescriptMember.Kind.Index,
            List(TypescriptType.Function(List(TypescriptType.Argument(key, keyType)), value)),
            visibility, static, readonly )

      else if at(t"[") then abort(TypescriptError(Reason.Unsupported(t"a computed property name")))
      else
        val getter = at(t"get") && !(ahead(1, t":")
            || ahead(1, t"("))

        val setter = at(t"set") && !(ahead(1, t":")
            || ahead(1, t"("))

        if getter || setter then next()

        val name = identifier()
        val optional = skip(t"?")

        if at(t"(") || at(t"<") then
          val typed = typeParameters()
          val parameters = parameterList()
          val result = if skip(t":") then typeExpression() else TypescriptType.Named(t"any")

          val kind =
            if getter then TypescriptMember.Kind.Getter
            else if setter then TypescriptMember.Kind.Setter
            else TypescriptMember.Kind.Method

          TypescriptMember
            ( name, kind, List(TypescriptType.Function(parameters, result, typed)), visibility,
              static, readonly, optional, isAbstract )
        else
          val typed = if skip(t":") then typeExpression() else TypescriptType.Named(t"any")

          TypescriptMember
            ( name, TypescriptMember.Kind.Property, List(typed), visibility, static, readonly,
              optional, isAbstract )

    // --- types -------------------------------------------------------------------------------

    private def typeParameters(): List[TypescriptType.Parameter] raises TypescriptError =
      if !skip(t"<") then Nil else
        val parameters = scala.collection.mutable.ListBuffer[TypescriptType.Parameter]()

        while !at(t">") && peek().present do
          if at(t"infer") then abort(TypescriptError(Reason.Unsupported(t"an `infer` binder")))
          val name = identifier()
          val bound: Optional[TypescriptType] = if skip(t"extends") then typeExpression() else Unset
          val default: Optional[TypescriptType] = if skip(t"=") then typeExpression() else Unset
          parameters += TypescriptType.Parameter(name, bound, default)
          skip(t",")

        expect(t">")
        List.from(parameters.toList)

    private def typeArguments(): List[TypescriptType] raises TypescriptError =
      if !skip(t"<") then Nil else
        val arguments = scala.collection.mutable.ListBuffer[TypescriptType]()

        while !at(t">") && peek().present do
          arguments += typeExpression()
          skip(t",")

        expect(t">")
        List.from(arguments.toList)

    private def typeList(): List[TypescriptType] raises TypescriptError =
      val types = scala.collection.mutable.ListBuffer[TypescriptType]()
      types += typeExpression()
      while skip(t",") do types += typeExpression()

      List.from(types.toList)

    private def parameterList(): List[TypescriptType.Argument] raises TypescriptError =
      expect(t"(")
      val parameters = scala.collection.mutable.ListBuffer[TypescriptType.Argument]()

      while !at(t")") && peek().present do
        val rest = skip(t"...")
        // Parameter modifiers (`public readonly x: T`) appear in constructor parameter
        // properties; they declare a member, but the parameter's own contract is its type.
        skip(t"public")
        skip(t"protected")
        skip(t"private")
        skip(t"readonly")
        val name = identifier()
        val optional = skip(t"?")
        val typed: Optional[TypescriptType] = if skip(t":") then typeExpression() else Unset
        // A default value makes a parameter optional; the value itself is behaviour, not
        // contract, so it is consumed and discarded.
        if skip(t"=") then skipDefault()
        parameters += TypescriptType.Argument(name, typed, optional, rest)
        skip(t",")

      expect(t")")
      List.from(parameters.toList)

    private def skipDefault(): Unit =
      var depth = 0

      while peek().present && !(depth == 0 && (at(t",") || at(t")"))) do
        if at(t"(") || at(t"[") || at(t"{") then depth += 1
        if at(t")") || at(t"]") || at(t"}") then depth -= 1
        next()

      ()

    def typeExpression(): TypescriptType raises TypescriptError =
      // A leading `|` or `&` is legal and purely cosmetic.
      skip(t"|")
      skip(t"&")

      val first = intersection()

      if !at(t"|") then first else
        val members = scala.collection.mutable.ListBuffer[TypescriptType]()
        members += first
        while skip(t"|") do members += intersection()

        TypescriptType.Union(List.from(members.toList))

    private def intersection(): TypescriptType raises TypescriptError =
      val first = suffixed()

      if !at(t"&") then first else
        val members = scala.collection.mutable.ListBuffer[TypescriptType]()
        members += first
        while skip(t"&") do members += suffixed()

        TypescriptType.Intersection(List.from(members.toList))

    // `T[]`, `T[][]` and `T[K]` all suffix a primary type, and they chain.
    private def suffixed(): TypescriptType raises TypescriptError =
      var result = primary()

      while at(t"[") do
        expect(t"[")

        if skip(t"]") then result = TypescriptType.Array(result)
        else
          val index = typeExpression()
          expect(t"]")
          result = TypescriptType.Indexed(result, index)

      if skip(t"is") then
        result match
          case TypescriptType.Named(name, Nil) =>
            TypescriptType.Predicate(name, typeExpression())

          case _ =>
            abort(TypescriptError(Reason.Syntax(t"a type predicate needs a parameter name", here)))
      else result

    private def primary(): TypescriptType raises TypescriptError =
      if at(t"(") then
        // Either a parenthesised type or a function type. They are distinguished only by what
        // follows the closing parenthesis, and `(a: T)` is not a valid type on its own, so the
        // parser reads the parenthesised form as a parameter list first and rewinds if no `=>`
        // follows.
        val mark = position

        val function: Optional[TypescriptType] =
          try
            val parameters = parameterList()
            if skip(t"=>") then TypescriptType.Function(parameters, typeExpression()) else Unset
          catch case _: Exception => Unset

        function match
          case function: TypescriptType => function

          case _ =>
            position = mark
            expect(t"(")
            val inner = typeExpression()
            expect(t")")
            inner

      else if at(t"<") then
        val typed = typeParameters()
        val parameters = parameterList()
        expect(t"=>")

        TypescriptType.Function(parameters, typeExpression(), typed)

      else if at(t"new") then
        next()
        val typed = typeParameters()
        val parameters = parameterList()
        expect(t"=>")

        TypescriptType.Function(parameters, typeExpression(), typed, construct = true)

      else if at(t"{") then
        expect(t"{")
        val members = memberList()
        expect(t"}")

        TypescriptType.Object(members)

      else if at(t"[") then
        expect(t"[")
        val members = scala.collection.mutable.ListBuffer[TypescriptType]()
        val names = scala.collection.mutable.ListBuffer[Optional[Text]]()

        while !at(t"]") && peek().present do
          skip(t"...")
          skip(t"readonly")

          // A labelled tuple element (`[first: A, second: B]`) names a position; the name is
          // documentation to TypeScript, so it is kept but never distinguishes a type.
          val label: Optional[Text] =
            if ahead(1, t":") then
              val name = identifier()
              expect(t":")
              name
            else Unset

          names += label
          members += typeExpression()
          skip(t"?")
          skip(t",")

        expect(t"]")

        TypescriptType.Tuple(List.from(members.toList), List.from(names.toList))

      else if at(t"keyof") then
        next()
        TypescriptType.Keyof(primary())

      else if at(t"typeof") then
        next()
        TypescriptType.Typeof(qualifiedName())

      else if at(t"infer") then abort(TypescriptError(Reason.Unsupported(t"an `infer` binder")))
      else if at(t"asserts") then
        abort(TypescriptError(Reason.Unsupported(t"an assertion signature")))
      else if at(t"unique") then abort(TypescriptError(Reason.Unsupported(t"a `unique symbol`")))
      else peek() match
        case Token.Str(value)  => { next(); TypescriptType.Literal(value,
            TypescriptType.LiteralKind.String) }

        case Token.Num(value)  => { next(); TypescriptType.Literal(value,
            TypescriptType.LiteralKind.Number) }

        case Token.Punct(t"-") =>
          next()

          peek() match
            case Token.Num(value) =>
              next()
              TypescriptType.Literal(t"-$value", TypescriptType.LiteralKind.Number)

            case _ => abort(TypescriptError(Reason.Syntax(t"expected a number", here)))

        case Token.Word(word) if word == t"true" || word == t"false" =>
          next()
          TypescriptType.Literal(word, TypescriptType.LiteralKind.Boolean)

        case Token.Word(_) =>
          val name = qualifiedName()
          val arguments = typeArguments()
          val named = TypescriptType.Named(name, arguments)

          // A conditional type is recognised here, where its `extends` follows a type rather
          // than a declaration name, and rejected rather than approximated.
          if at(t"extends")
          then abort(TypescriptError(Reason.Unsupported(t"a conditional type")))
          else named

        case _ => abort(TypescriptError(Reason.Syntax(t"expected a type", here)))

    private def qualifiedName(): Text raises TypescriptError =
      val parts = scala.collection.mutable.ListBuffer[Text]()
      parts += identifier()
      while skip(t".") do parts += identifier()

      parts.toList.map(_.s).mkString(".").tt

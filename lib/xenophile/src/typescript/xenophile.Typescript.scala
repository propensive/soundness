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
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

import Typescript.Error.Reason

// The TypeScript ecosystem: a set of `Interoperable` markers associating Scala types with the
// foreign types `TypescriptDialect` reads from `.ts` definition files. No runtime representation is
// involved — these only record the type correspondence used for type-checking and conversion.
object Typescript:
  given text: (Text is Interoperable in Typescript of "string") =
    Interoperable[Text, Typescript, "string"]()

  given int: (Int is Interoperable in Typescript of "number") =
    Interoperable[Int, Typescript, "number"]()

  given boolean: (Boolean is Interoperable in Typescript of "boolean") =
    Interoperable[Boolean, Typescript, "boolean"]()

  // A TypeScript `T[]` (i.e. `Array<T>`) corresponds to a Scala `List` of the element type.
  given list: [element, topic]
  =>  ( element is Interoperable in Typescript of topic )
  =>  ( List[element] is Interoperable in Typescript of ("Array" over topic) ) =
    Interoperable[List[element], Typescript, ("Array" over topic)]()

  // A TypeScript `Map<K, V>` corresponds to a Scala `Map`.
  given map: [key, value, keyTopic, valueTopic]
  =>  ( key is Interoperable in Typescript of keyTopic,
        value is Interoperable in Typescript of valueTopic )
  =>  ( Map[key, value] is Interoperable in Typescript of ("Map" over (keyTopic, valueTopic)) ) =
    Interoperable[Map[key, value], Typescript, ("Map" over (keyTopic, valueTopic))]()

  // TypeScript `undefined` (produced by reading `T?` as `T | undefined`) is the absent `Optional`.
  given undefined: (Unset.type is Interoperable in Typescript of "undefined") =
    Interoperable[Unset.type, Typescript, "undefined"]()

  // A TypeScript `T?` (read as `T | undefined`) corresponds to a Scala `Optional`. The `Mandatable`
  // constraint identifies the mandatory type `inner`, so the instance applies only to genuine
  // optionals and never competes with `inner`'s instance.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner, topic]
  =>  ( inner is Interoperable in Typescript of topic )
  =>  ( value is Interoperable in Typescript of (topic | "undefined") ) =
    Interoperable[value, Typescript, (topic | "undefined")]()

  // TypescriptDeclaration → Typescript.Declaration
  object Declaration:
    // A declaration's namespace path, outermost first: `declare namespace a { namespace b { … } }`
    // gives an inner declaration the scope `a.b`. Two declarations of the same name in different
    // namespaces are different contracts, so the scope is part of every key.
    type Scope = List[Text]

    extension (scope: Scope)
      def qualify(name: Text): Text =
        if scope.stdlib.isEmpty then name else t"${scope.join(t".")}.$name"

  // What every declaration has in common. Declared abstractly here rather than as methods on the
  // enum, so that each case's own parameters implement them: an enum whose cases redeclare a
  // member of the enum body would need an `override` on every parameter.
  sealed trait Declared:
    def name: Text
    def scope: Typescript.Declaration.Scope
    def exported: Boolean

  // A top-level declaration of a `.d.ts` file.
  //
  // `exported` records whether the declaration is reachable by a consumer of the module: in a
  // module (a file with any top-level `import`/`export`), only exported declarations are; in a
  // legacy global script every top-level declaration is. Resolving that distinction is the
  // parser's job, so by the time a declaration reaches a discipline the flag means what it says.
  enum Declaration extends Declared:
    case Interface
      ( name:     Text,
        scope:    Typescript.Declaration.Scope,
        typed:    List[Typescript.Type.Parameter],
        extending: List[Typescript.Type],
        members:  List[Typescript.Member],
        exported: Boolean )

    case Class
      ( name:       Text,
        scope:      Typescript.Declaration.Scope,
        typed:      List[Typescript.Type.Parameter],
        extending:  Optional[Typescript.Type],
        implements: List[Typescript.Type],
        members:    List[Typescript.Member],
        isAbstract: Boolean,
        exported:   Boolean )

    case Alias
      ( name:     Text,
        scope:    Typescript.Declaration.Scope,
        typed:    List[Typescript.Type.Parameter],
        target:   Typescript.Type,
        exported: Boolean )

    // An enum's members are its contract, and a `const enum` is inlined into consumers at *their*
    // compile time, which is a materially different guarantee — so the flag is carried, not
    // discarded.
    case Enumeration
      ( name:     Text,
        scope:    Typescript.Declaration.Scope,
        members:  List[(Text, Optional[Text])],
        constant: Boolean,
        exported: Boolean )

    case Function
      ( name:       Text,
        scope:      Typescript.Declaration.Scope,
        signatures: List[Typescript.Type],
        exported:   Boolean )

    case Variable
      ( name:     Text,
        scope:    Typescript.Declaration.Scope,
        typed:    Optional[Typescript.Type],
        constant: Boolean,
        exported: Boolean )

    // The members a declaration presents. Named apart from the `members` parameter that two of the
    // cases carry, which an enum-level method of the same name would collide with.
    def declaredMembers: List[Typescript.Member] = this match
      case Interface(_, _, _, _, members, _)   => members
      case Class(_, _, _, _, _, members, _, _) => members
      case _                                   => Nil

    import Typescript.Declaration.qualify
    def key: Text = scope.qualify(name)

  // TypescriptError → Typescript.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Syntax(detail: Text, near: Text)  extends Reason(1)
      case Unsupported(construct: Text)      extends Reason(2)
      case Duplicate(name: Text)             extends Reason(3)

    given communicable: Reason is Communicable =
      case Reason.Syntax(detail, near) => m"$detail, near $near"
      case Reason.Unsupported(construct) =>
        m"the construct $construct is outside the grammar this parser accepts"

      case Reason.Duplicate(name) => m"the declaration $name appears twice in the same scope"

  // A `.d.ts` source could not be read. `Unsupported` is deliberately an error and not a silent
  // skip: a declaration file is a contract, and a parser that quietly drops what it does not
  // recognise reports a smaller contract than the file declares — which would make any
  // compatibility claim computed from it unsound.
  case class Error(reason: Typescript.Error.Reason)(using Diagnostics)
  extends fulminate.Error(643, reason.number)(m"the TypeScript declarations could not be read because $reason")

  // TypescriptMember → Typescript.Member
  object Member:
    // What the member *is*, which decides how a consumer may address it. A property and a method of
    // the same name are different contracts even when their types agree, and a call signature is
    // addressable only by invoking the enclosing type.
    enum Kind:
      case Property, Method, Getter, Setter, Call, Construct, Index

    enum Visibility:
      case Public, Protected, Private

  // One member of an interface, class or inline object type.
  //
  // Overloads are *not* merged: TypeScript resolves a call against the declared signatures in
  // order, so a member holds the list it was declared with and the order is semantic.
  case class Member
    ( name:       Text,
      kind:       Typescript.Member.Kind,
      signatures: List[Typescript.Type],
      visibility: Typescript.Member.Visibility = Typescript.Member.Visibility.Public,
      static:     Boolean = false,
      readonly:   Boolean = false,
      optional:   Boolean = false,
      isAbstract: Boolean = false ):

    // The key a member is addressed by within its owning declaration. Getters and setters share a
    // name with a property but are distinct contracts, and index and call signatures have no name
    // of their own, so each kind contributes its own selector shape.
    def selector: Text = kind match
      case Typescript.Member.Kind.Property  => name
      case Typescript.Member.Kind.Method    => name
      case Typescript.Member.Kind.Getter    => t"get $name"
      case Typescript.Member.Kind.Setter    => t"set $name"
      case Typescript.Member.Kind.Call      => t"()"
      case Typescript.Member.Kind.Construct => t"new()"
      case Typescript.Member.Kind.Index     => t"[]"

    // A `private` member is not part of any consumer's contract, and a TypeScript consumer cannot
    // name it. `protected` is, since a subclass may.
    def visible: Boolean = visibility != Typescript.Member.Visibility.Private

  // TypescriptType → Typescript.Type
  object Type:
    // A literal type's kind, kept apart from its text because `"1"` and `1` are different types
    // whose source forms differ only in quoting, which the lexer has already removed.
    enum LiteralKind:
      case String, Number, Boolean

    // A type parameter's binder: its bound (`T extends U`) and its default (`T = U`). The name is
    // carried for diagnostics and for resolving references within the binder's scope; a canonical
    // encoding is expected to replace it with a positional index.
    case class Parameter(name: Text, bound: Optional[Typescript.Type], default: Optional[Typescript.Type])

    // A value parameter of a function, method or constructor. `rest` marks `...args: T[]`, whose
    // arity is unbounded, and `optional` marks `a?: T`, which callers may omit — both change what
    // call sites are legal, so both are part of the contract.
    case class Argument
      ( name:     Text,
        typed:    Optional[Typescript.Type],
        optional: Boolean = false,
        rest:     Boolean = false )

  // A TypeScript type expression.
  //
  // The vocabulary is deliberately closed: `Typescript.Parser` raises rather than guessing when it
  // meets a construct not represented here, so a `.d.ts` file using one is rejected loudly instead
  // of being read as a smaller interface than it declares. That property is what lets a discipline
  // atomize these values at all (LIRA §11.2 requirement 3).
  enum Type:
    case Named(name: Text, arguments: List[Typescript.Type] = Nil)
    case Literal(value: Text, kind: Typescript.Type.LiteralKind)
    case Union(members: List[Typescript.Type])
    case Intersection(members: List[Typescript.Type])
    case Tuple(members: List[Typescript.Type], names: List[Optional[Text]] = Nil)
    case Array(element: Typescript.Type)
    case Object(members: List[Typescript.Member])
    case Keyof(target: Typescript.Type)
    case Typeof(target: Text)
    case Indexed(target: Typescript.Type, index: Typescript.Type)
    case Predicate(parameter: Text, target: Typescript.Type)

    case Function
      ( parameters: List[Typescript.Type.Argument],
        result:     Typescript.Type,
        typed:      List[Typescript.Type.Parameter] = Nil,
        construct:  Boolean = false )

    // A stable rendering, used in diagnostics. It is *not* the canonical encoding: the encoding a
    // discipline hashes is structural and binder-name-free, and lives with the discipline.
    //
    // Rendering runs over the `stdlib` view rather than the opaque collections' `map`/`join`. An
    // extension method applied under a still-uninstantiated type variable is the shape that trips
    // the compiler's `wildApprox` assertion (scala/scala3#24824), and a recursive renderer over a
    // generic collection reaches it reliably.
    def text: Text =
      def render(types: List[Typescript.Type], separator: String): String =
        types.stdlib.map { typed => typed.text.s }.mkString(separator)

      val rendered: String = this match
        case Named(name, Nil)       => name.s
        case Literal(value, _)      => value.s
        case Union(members)         => render(members, " | ")
        case Intersection(members)  => render(members, " & ")
        case Array(element)         => element.text.s+"[]"
        case Keyof(target)          => "keyof "+target.text.s
        case Typeof(target)         => "typeof "+target.s
        case Named(name, arguments) => name.s+"<"+render(arguments, ", ")+">"
        case Tuple(members, _)      => "["+render(members, ", ")+"]"
        case Indexed(target, index) => target.text.s+"["+index.text.s+"]"
        case Predicate(name, target) => name.s+" is "+target.text.s

        case Object(members) =>
          "{ "+members.stdlib.map { member => member.name.s }.mkString("; ")+" }"

        case Function(parameters, result, _, construct) =>
          val arguments = parameters.stdlib.map: parameter =>
            parameter.name.s+": "+parameter.typed.lay("any") { value => value.text.s }

          (if construct then "new " else "")+"("+arguments.mkString(", ")+") => "+result.text.s

      rendered.tt

  // TypescriptParser → Typescript.Parser
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
  object Parser:

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

    def lex(source: Text): List[Token] raises Typescript.Error =
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
      def string(start: Int, quote: Char): Int raises Typescript.Error =
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
        then abort(Typescript.Error(Reason.Syntax(t"the string literal is unterminated", t"$quote")))

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
          abort(Typescript.Error(Reason.Unsupported(t"a template literal type")))
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
              else abort(Typescript.Error(Reason.Syntax(t"unexpected character", char.toString.tt)))

      List.from(tokens.toList)

    def parse(source: Text): List[Typescript.Declaration] raises Typescript.Error =
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

      def expect(text: Text): Unit raises Typescript.Error =
        if !skip(text)
        then abort(Typescript.Error(Reason.Syntax(t"expected $text", here)))

      def identifier(): Text raises Typescript.Error = next() match
        case Token.Word(text) => text
        case Token.Str(text)  => text
        case Token.Num(text)  => text
        case _ => abort(Typescript.Error(Reason.Syntax(t"expected a name", here)))

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

      def declarations(): List[Typescript.Declaration] raises Typescript.Error =
        val module = moduleForm
        val result = scala.collection.mutable.ListBuffer[Typescript.Declaration]()
        block(Nil, module, result, ambient = false)

        val seen = scala.collection.mutable.HashSet[Text]()

        // Interfaces and namespaces merge in TypeScript; classes, aliases and enums do not. A
        // repeated non-mergeable name is a source error, not something to resolve by last-wins.
        def mergeable(declaration: Typescript.Declaration): Boolean = declaration match
          case _: Typescript.Declaration.Interface => true
          case _: Typescript.Declaration.Function  => true
          case _                                  => false

        result.toList.foreach: declaration =>
          val key = declaration.key
          if !mergeable(declaration) && !seen.add(key)
          then abort(Typescript.Error(Reason.Duplicate(key)))

        List.from(result.toList)

      // `ambient` marks a namespace body, where every declaration is exported implicitly: an
      // ambient namespace has no notion of a private member, so its contents are as reachable as
      // the namespace itself.
      private def block
        ( scope:   Typescript.Declaration.Scope,
          module:  Boolean,
          into:    scala.collection.mutable.ListBuffer[Typescript.Declaration],
          ambient: Boolean )
      :   Unit raises Typescript.Error =

        while peek().present && !at(t"}") do
          if at(t"}") then () else declaration(scope, module, into, ambient)

        ()

      private def declaration
        ( scope:   Typescript.Declaration.Scope,
          module:  Boolean,
          into:    scala.collection.mutable.ListBuffer[Typescript.Declaration],
          ambient: Boolean )
      :   Unit raises Typescript.Error =

        if skip(t";") then ()
        else if at(t"import") then skipStatement()
        else if at(t"@") then abort(Typescript.Error(Reason.Unsupported(t"a decorator")))
        else
          val exported = skip(t"export")

          // `export default …`, `export = …` and `export { … }` re-export existing names; they
          // change nothing about the declarations themselves, which are parsed where they stand.
          if exported && (at(t"default") || at(t"=") || at(t"{") || at(t"*")) then skipStatement()
          else
            skip(t"declare")
            declared(scope, module, into, ambient, exported || ambient || !module)

      private def declared
        ( scope:   Typescript.Declaration.Scope,
          module:  Boolean,
          into:    scala.collection.mutable.ListBuffer[Typescript.Declaration],
          ambient: Boolean,
          visible: Boolean )
      :   Unit raises Typescript.Error =

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
        else abort(Typescript.Error(Reason.Unsupported(t"a top-level ${here} declaration")))

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

      private def interfaceDeclaration(scope: Typescript.Declaration.Scope, exported: Boolean)
      :   Typescript.Declaration raises Typescript.Error =

        expect(t"interface")
        val name = identifier()
        val typed = typeParameters()
        val extending = if skip(t"extends") then typeList() else Nil
        expect(t"{")
        val members = memberList()
        expect(t"}")

        Typescript.Declaration.Interface(name, scope, typed, extending, members, exported)

      private def classDeclaration(scope: Typescript.Declaration.Scope, exported: Boolean)
      :   Typescript.Declaration raises Typescript.Error =

        val isAbstract = skip(t"abstract")
        expect(t"class")
        val name = identifier()
        val typed = typeParameters()
        val extending: Optional[Typescript.Type] = if skip(t"extends") then typeExpression() else Unset
        val implements = if skip(t"implements") then typeList() else Nil
        expect(t"{")
        val members = memberList()
        expect(t"}")

        Typescript.Declaration.Class
          (name, scope, typed, extending, implements, members, isAbstract, exported)

      private def aliasDeclaration(scope: Typescript.Declaration.Scope, exported: Boolean)
      :   Typescript.Declaration raises Typescript.Error =

        expect(t"type")
        val name = identifier()
        val typed = typeParameters()
        expect(t"=")
        val target = typeExpression()
        separator()

        Typescript.Declaration.Alias(name, scope, typed, target, exported)

      private def enumDeclaration(scope: Typescript.Declaration.Scope, exported: Boolean)
      :   Typescript.Declaration raises Typescript.Error =

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

        Typescript.Declaration.Enumeration(name, scope, List.from(members.toList), constant, exported)

      private def functionDeclaration(scope: Typescript.Declaration.Scope, exported: Boolean)
      :   Typescript.Declaration raises Typescript.Error =

        expect(t"function")
        val name = identifier()
        val typed = typeParameters()
        val parameters = parameterList()
        val result = if skip(t":") then typeExpression() else Typescript.Type.Named(t"void")
        separator()

        Typescript.Declaration.Function
          (name, scope, List(Typescript.Type.Function(parameters, result, typed)), exported)

      private def variableDeclaration(scope: Typescript.Declaration.Scope, exported: Boolean)
      :   Typescript.Declaration raises Typescript.Error =

        val constant = at(t"const")
        next()
        val name = identifier()
        val typed: Optional[Typescript.Type] = if skip(t":") then typeExpression() else Unset
        separator()

        Typescript.Declaration.Variable(name, scope, typed, constant, exported)

      // --- members -----------------------------------------------------------------------------

      private def memberList(): List[Typescript.Member] raises Typescript.Error =
        val members = scala.collection.mutable.ListBuffer[Typescript.Member]()

        while !at(t"}") && peek().present do
          member().let { value => members += value }
          separator()

        // Overload groups are folded here rather than in the grammar: consecutive signatures under
        // one name and kind are one member with several signatures, in declaration order, because
        // that order is what TypeScript's overload resolution reads.
        val merged = scala.collection.mutable.LinkedHashMap[Text, Typescript.Member]()

        members.toList.foreach: member =>
          val selector = member.selector

          merged.get(selector) match
            case scala.Some(existing) =>
              val signatures: List[Typescript.Type] =
                List.from(existing.signatures.stdlib ++ member.signatures.stdlib)

              merged.put(selector, existing.copy(signatures = signatures))

            case scala.None => merged.put(selector, member)

        List.from(merged.values.toList)

      private def member(): Optional[Typescript.Member] raises Typescript.Error =
        if skip(t";") then Unset
        else if at(t"@") then abort(Typescript.Error(Reason.Unsupported(t"a decorator")))
        else declaredMember()

      private def declaredMember(): Typescript.Member raises Typescript.Error =
        var visibility = Typescript.Member.Visibility.Public
        var static = false
        var readonly = false
        var isAbstract = false

        var scanning = true

        while scanning do
          if at(t"public") then { next(); visibility = Typescript.Member.Visibility.Public }
          else if at(t"protected") then { next(); visibility = Typescript.Member.Visibility.Protected }
          else if at(t"private") then { next(); visibility = Typescript.Member.Visibility.Private }
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
          val result = if skip(t":") then typeExpression() else Typescript.Type.Named(t"any")

          Typescript.Member
            ( t"", Typescript.Member.Kind.Call,
              List(Typescript.Type.Function(parameters, result, typed)),
              visibility, static, readonly )

        // A construct signature `new (…): T`.
        else if at(t"new") && (ahead(1, t"(") || peek(1)
            == Optional(Token.Punct(t"<"))) then
          next()
          val typed = typeParameters()
          val parameters = parameterList()
          val result = if skip(t":") then typeExpression() else Typescript.Type.Named(t"any")

          Typescript.Member
            ( t"", Typescript.Member.Kind.Construct,
              List(Typescript.Type.Function(parameters, result, typed, construct = true)),
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

          Typescript.Member
            ( key, Typescript.Member.Kind.Index,
              List(Typescript.Type.Function(List(Typescript.Type.Argument(key, keyType)), value)),
              visibility, static, readonly )

        // A mapped type (`[K in T]: U`) is refused under its own name: it fails the index-signature
        // test above because `in` follows the binder where `:` would, but calling it a computed
        // property name would misdirect whoever reads the diagnostic.
        else if at(t"[") && ahead(2, t"in")
        then abort(Typescript.Error(Reason.Unsupported(t"a mapped type")))
        else if at(t"[") then abort(Typescript.Error(Reason.Unsupported(t"a computed property name")))
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
            val result = if skip(t":") then typeExpression() else Typescript.Type.Named(t"any")

            val kind =
              if getter then Typescript.Member.Kind.Getter
              else if setter then Typescript.Member.Kind.Setter
              else Typescript.Member.Kind.Method

            Typescript.Member
              ( name, kind, List(Typescript.Type.Function(parameters, result, typed)), visibility,
                static, readonly, optional, isAbstract )
          else
            val typed = if skip(t":") then typeExpression() else Typescript.Type.Named(t"any")

            Typescript.Member
              ( name, Typescript.Member.Kind.Property, List(typed), visibility, static, readonly,
                optional, isAbstract )

      // --- types -------------------------------------------------------------------------------

      private def typeParameters(): List[Typescript.Type.Parameter] raises Typescript.Error =
        if !skip(t"<") then Nil else
          val parameters = scala.collection.mutable.ListBuffer[Typescript.Type.Parameter]()

          while !at(t">") && peek().present do
            if at(t"infer") then abort(Typescript.Error(Reason.Unsupported(t"an `infer` binder")))
            val name = identifier()
            val bound: Optional[Typescript.Type] = if skip(t"extends") then typeExpression() else Unset
            val default: Optional[Typescript.Type] = if skip(t"=") then typeExpression() else Unset
            parameters += Typescript.Type.Parameter(name, bound, default)
            skip(t",")

          expect(t">")
          List.from(parameters.toList)

      private def typeArguments(): List[Typescript.Type] raises Typescript.Error =
        if !skip(t"<") then Nil else
          val arguments = scala.collection.mutable.ListBuffer[Typescript.Type]()

          while !at(t">") && peek().present do
            arguments += typeExpression()
            skip(t",")

          expect(t">")
          List.from(arguments.toList)

      private def typeList(): List[Typescript.Type] raises Typescript.Error =
        val types = scala.collection.mutable.ListBuffer[Typescript.Type]()
        types += typeExpression()
        while skip(t",") do types += typeExpression()

        List.from(types.toList)

      private def parameterList(): List[Typescript.Type.Argument] raises Typescript.Error =
        expect(t"(")
        val parameters = scala.collection.mutable.ListBuffer[Typescript.Type.Argument]()

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
          val typed: Optional[Typescript.Type] = if skip(t":") then typeExpression() else Unset
          // A default value makes a parameter optional; the value itself is behaviour, not
          // contract, so it is consumed and discarded.
          if skip(t"=") then skipDefault()
          parameters += Typescript.Type.Argument(name, typed, optional, rest)
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

      def typeExpression(): Typescript.Type raises Typescript.Error =
        // A leading `|` or `&` is legal and purely cosmetic.
        skip(t"|")
        skip(t"&")

        val first = intersection()

        if !at(t"|") then first else
          val members = scala.collection.mutable.ListBuffer[Typescript.Type]()
          members += first
          while skip(t"|") do members += intersection()

          Typescript.Type.Union(List.from(members.toList))

      private def intersection(): Typescript.Type raises Typescript.Error =
        val first = suffixed()

        if !at(t"&") then first else
          val members = scala.collection.mutable.ListBuffer[Typescript.Type]()
          members += first
          while skip(t"&") do members += suffixed()

          Typescript.Type.Intersection(List.from(members.toList))

      // `T[]`, `T[][]` and `T[K]` all suffix a primary type, and they chain.
      private def suffixed(): Typescript.Type raises Typescript.Error =
        var result = primary()

        while at(t"[") do
          expect(t"[")

          if skip(t"]") then result = Typescript.Type.Array(result)
          else
            val index = typeExpression()
            expect(t"]")
            result = Typescript.Type.Indexed(result, index)

        if skip(t"is") then
          result match
            case Typescript.Type.Named(name, Nil) =>
              Typescript.Type.Predicate(name, typeExpression())

            case _ =>
              abort(Typescript.Error(Reason.Syntax(t"a type predicate needs a parameter name", here)))
        else result

      private def primary(): Typescript.Type raises Typescript.Error =
        if at(t"(") then
          // Either a parenthesised type or a function type. They are distinguished only by what
          // follows the closing parenthesis, and `(a: T)` is not a valid type on its own, so the
          // parser reads the parenthesised form as a parameter list first and rewinds if no `=>`
          // follows.
          val mark = position

          val function: Optional[Typescript.Type] =
            try
              val parameters = parameterList()
              if skip(t"=>") then Typescript.Type.Function(parameters, typeExpression()) else Unset
            catch case _: Exception => Unset

          function match
            case function: Typescript.Type => function

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

          Typescript.Type.Function(parameters, typeExpression(), typed)

        else if at(t"new") then
          next()
          val typed = typeParameters()
          val parameters = parameterList()
          expect(t"=>")

          Typescript.Type.Function(parameters, typeExpression(), typed, construct = true)

        else if at(t"{") then
          expect(t"{")
          val members = memberList()
          expect(t"}")

          Typescript.Type.Object(members)

        else if at(t"[") then
          expect(t"[")
          val members = scala.collection.mutable.ListBuffer[Typescript.Type]()
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

          Typescript.Type.Tuple(List.from(members.toList), List.from(names.toList))

        else if at(t"keyof") then
          next()
          Typescript.Type.Keyof(primary())

        else if at(t"typeof") then
          next()
          Typescript.Type.Typeof(qualifiedName())

        else if at(t"infer") then abort(Typescript.Error(Reason.Unsupported(t"an `infer` binder")))
        else if at(t"asserts") then
          abort(Typescript.Error(Reason.Unsupported(t"an assertion signature")))
        else if at(t"unique") then abort(Typescript.Error(Reason.Unsupported(t"a `unique symbol`")))
        else peek() match
          case Token.Str(value)  => { next(); Typescript.Type.Literal(value,
              Typescript.Type.LiteralKind.String) }

          case Token.Num(value)  => { next(); Typescript.Type.Literal(value,
              Typescript.Type.LiteralKind.Number) }

          case Token.Punct(t"-") =>
            next()

            peek() match
              case Token.Num(value) =>
                next()
                Typescript.Type.Literal(t"-$value", Typescript.Type.LiteralKind.Number)

              case _ => abort(Typescript.Error(Reason.Syntax(t"expected a number", here)))

          case Token.Word(word) if word == t"true" || word == t"false" =>
            next()
            Typescript.Type.Literal(word, Typescript.Type.LiteralKind.Boolean)

          case Token.Word(_) =>
            val name = qualifiedName()
            val arguments = typeArguments()
            val named = Typescript.Type.Named(name, arguments)

            // A conditional type is recognised here, where its `extends` follows a type rather
            // than a declaration name, and rejected rather than approximated.
            if at(t"extends")
            then abort(Typescript.Error(Reason.Unsupported(t"a conditional type")))
            else named

          case _ => abort(Typescript.Error(Reason.Syntax(t"expected a type", here)))

      private def qualifiedName(): Text raises Typescript.Error =
        val parts = scala.collection.mutable.ListBuffer[Text]()
        parts += identifier()
        while skip(t".") do parts += identifier()

        parts.toList.map(_.s).mkString(".").tt

trait Typescript extends Ecosystem:
  type Grammar = TypescriptDialect.type
  type Emission = "xenophile.JsInvoke"

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
package xenophile

import anticipation.*
import gossamer.*
import vacuous.*

// A minimal grammar for C header files (the `extern "C"` subset that Rust's `cbindgen`, and C/C++
// public headers, expose): `struct` definitions become navigable foreign types whose members are
// their fields, and top-level function prototypes become members of a synthetic `"library"` type
// representing the loaded library. Preprocessor lines and comments are skipped; the rest of C is
// not interpreted.
object CHeaderDialect extends Dialect:
  val library: Text = t"library"

  def parse(source: Text): Map[Text, Map[Text, Signature]] =
    declarations(tokenize(source.s), Map(), Map())

  private def punctuation(char: Char): Boolean =
    char == '{' || char == '}' || char == '(' || char == ')' || char == ';' || char == ',' ||
      char == '*' || char == '[' || char == ']'

  // Splits the source into identifier and punctuation tokens, skipping whitespace, `//` and
  // `/* … */` comments, and `#…` preprocessor lines (the only use of `#` in a header).
  private def tokenize(source: String): List[String] =
    def skipLine(index: Int): Int =
      if index >= source.length || source.charAt(index) == '\n' then index + 1
      else skipLine(index + 1)

    def skipBlock(index: Int): Int =
      if index + 1 >= source.length then source.length
      else if source.charAt(index) == '*' && source.charAt(index + 1) == '/' then index + 2
      else skipBlock(index + 1)

    def recur(index: Int, current: String, tokens: List[String]): List[String] =
      val flushed = if current.isEmpty then tokens else current :: tokens

      if index >= source.length then flushed.reverse else
        val char = source.charAt(index)
        val next = if index + 1 < source.length then source.charAt(index + 1) else ' '

        if char == '#' || (char == '/' && next == '/') then recur(skipLine(index), "", flushed)
        else if char == '/' && next == '*' then recur(skipBlock(index + 2), "", flushed)
        else if char.isLetterOrDigit || char == '_' then recur(index + 1, current + char, tokens)
        else if punctuation(char) then recur(index + 1, "", char.toString :: flushed)
        else recur(index + 1, "", flushed)

    recur(0, "", Nil)

  private def isWord(token: String): Boolean =
    token.nonEmpty && (token.charAt(0).isLetter || token.charAt(0) == '_')

  private def isKeyword(token: String): Boolean = token match
    case "unsigned" | "signed" | "long" | "short" | "int" | "char" => true
    case "double" | "float" | "void" | "bool" | "_Bool"            => true
    case _                                                         => false

  // Reads a type and an optional declarator name: `int x` → (int, "x"), `const char* s` →
  // (string, "s"), and an abstract `int` / `char*` → (…, Unset). The base type is its leading
  // keywords (`unsigned int`) or single identifier (`Point`, `size_t`), then trailing `*`s; a
  // lone `char*` / `const char*` is the C-string type `"string"`, any other `T*` is `"ptr" over T`.
  private def declarator(tokens: List[String]): (ForeignType, Optional[Text], List[String]) =
    def typeWords(todo: List[String], acc: List[String]): (List[String], List[String]) =
      todo match
        case ("const" | "volatile" | "struct" | "enum" | "union") :: more =>
          typeWords(more, acc)

        case word :: more if isKeyword(word) =>
          typeWords(more, word :: acc)

        case word :: more if isWord(word) && acc.isEmpty =>
          (List(word), more)

        case _ =>
          (acc.reverse, todo)

    def stars(todo: List[String], count: Int): (Int, List[String]) = todo match
      case "*" :: more => stars(more, count + 1)
      case _           => (count, todo)

    val (words, rest0) = typeWords(tokens, Nil)
    val (pointers, rest1) = stars(rest0, 0)

    val (name, rest) = rest1 match
      case word :: more if isWord(word) => (word.tt, more)
      case _                            => (Unset, rest1)

    val base = words.mkString(" ").tt

    val foreign =
      if pointers == 0 then ForeignType.Named(base)
      else if base == t"char" && pointers == 1 then ForeignType.Named(t"string")
      else ForeignType.Applied(t"ptr", List(ForeignType.Named(base)))

    (foreign, name, rest)

  // Walks the top-level declarations, accumulating named types (`structs`) and the synthetic
  // `"library"` type's function members (`functions`).
  private def declarations
    ( tokens:    List[String],
      structs:   Map[Text, Map[Text, Signature]],
      functions: Map[Text, Signature] )
  :   Map[Text, Map[Text, Signature]] =

    tokens match
      case Nil =>
        if functions.isEmpty then structs else structs.updated(library, functions)

      // `struct Name { … };` and `typedef struct Name? { … } Alias;`
      case ("struct" | "typedef") :: rest =>
        val rest1 = rest match
          case "struct" :: more => more
          case more             => more

        rest1 match
          case name :: "{" :: more =>
            val (fields, after) = members(more, Map())
            val (alias, after2) = aliasName(after)

            declarations(after2, structs.updated(alias.or(name.tt), fields), functions)

          case "{" :: more =>
            val (fields, after) = members(more, Map())
            val (alias, after2) = aliasName(after)

            alias.lay(declarations(after2, structs, functions)): name =>
              declarations(after2, structs.updated(name, fields), functions)

          case _ =>
            declarations(skipStatement(rest1), structs, functions)

      // A top-level function prototype: `<type> <name> ( <params> ) ;`.
      case _ =>
        val (result, name, rest) = declarator(tokens)

        rest match
          case "(" :: more =>
            name.lay(declarations(skipStatement(more), structs, functions)): function =>
              val (params, after) = parameters(more, Nil)

              declarations
                ( skipStatement(after),
                  structs,
                  functions.updated(function, Signature(params, result)) )

          case _ =>
            declarations(skipStatement(tokens), structs, functions)

  // Reads a struct body's fields up to the closing `}`.
  private def members(tokens: List[String], acc: Map[Text, Signature])
  :   (Map[Text, Signature], List[String]) =

    tokens match
      case "}" :: rest =>
        (acc, rest)

      case Nil =>
        (acc, Nil)

      case _ =>
        val (kind, name, rest) = declarator(tokens)

        rest match
          case ";" :: more =>
            name.lay(members(more, acc)): field =>
              members(more, acc.updated(field, Signature(Unset, kind)))

          case _ =>
            members(skipStatement(tokens), acc)

  // After a struct's closing `}`, reads an optional `typedef` alias name before the `;`.
  private def aliasName(tokens: List[String]): (Optional[Text], List[String]) = tokens match
    case ";" :: rest         => (Unset, rest)
    case name :: ";" :: rest => (name.tt, rest)
    case _                   => (Unset, skipStatement(tokens))

  // Reads a function's parameter types up to the closing `)`; `(void)` denotes no parameters.
  private def parameters(tokens: List[String], acc: List[ForeignType])
  :   (Optional[List[ForeignType]], List[String]) =

    tokens match
      case ")" :: rest =>
        (acc.reverse, rest)

      case "void" :: ")" :: rest =>
        (acc.reverse, rest)

      case _ =>
        val (kind, _, rest) = declarator(tokens)

        rest match
          case "," :: more => parameters(more, kind :: acc)
          case ")" :: more => ((kind :: acc).reverse, more)
          case _           => (acc.reverse, skipStatement(rest))

  // Skips tokens up to and including the next `;` (to recover from constructs we do not model).
  private def skipStatement(tokens: List[String]): List[String] = tokens match
    case Nil         => Nil
    case ";" :: rest => rest
    case _ :: rest   => skipStatement(rest)

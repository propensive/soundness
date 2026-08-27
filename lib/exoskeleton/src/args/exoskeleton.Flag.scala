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
package exoskeleton

import scala.language.experimental.pureFunctions

import scala.caps
import scala.compiletime.*

import anticipation.*
import contingency.*
import fulminate.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

object Flag:
  def serialize(name: Text | Char): Text = name.absolve match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

  given showable: Flag is Showable = _.name.absolve match
    case name: Text => t"--$name"
    case name: Char => t"-$name"

  @targetName("make")
  def apply[topic]
    ( name:        Text | Char,
      repeatable:  Boolean           = false,
      aliases:     List[Text | Char] = Nil,
      description: Optional[Text]    = Unset,
      secret:      Boolean           = false )
    ( using erased defaulting: topic is Defaulting to Text )
  :   Flag of topic =

    new Flag(name, repeatable, aliases, description, secret):
      type Topic = topic


case class Flag
  ( name:        Text | Char,
    repeatable:  Boolean,
    aliases:     List[Text | Char],
    description: Optional[Text],
    secret:      Boolean )
extends Topical:
  def suggest(using interpretable: Topic is Interpretable, discoverable: Topic is Discoverable)
    ( using cli: Cli )
  :   Unit =

    cli.register(this, discoverable, interpretable.operandName)


  def matches(key: Argument): Boolean =
    val flag =
      if key().starts(t"--") then key().skip(2) else if key().starts(t"-")
      then key()(Sec) else Unset

    flag == name || aliases.stdlib.contains(flag)


  // Both `apply` and `require` dispatch on the erased `Effectful` capability, which only an
  // `execute` block provides. In the pure section they return a handle — a `Prospective` or a
  // `Requisite` — resolved eagerly from the arguments; inside `execute` they resolve directly,
  // to an `Optional` value and to the value itself respectively.
  transparent inline def apply()
    ( using cli:           Cli,
            interpreter:   Interpreter,
            interpretable: Topic is Interpretable,
            suggestions:   (? <: Topic) is Discoverable = Discoverable.noSuggestions )
  :   Prospective[Topic] | Optional[Topic] =

    cli.register(this, suggestions, interpretable.operandName)

    summonFrom:
      case _: Effectful => cli.parameter[Topic](this)
      case _            => Prospective(this, cli.parameter[Topic](this))


  // Requiring a flag in the pure section never fails fast: there may be several missing
  // requirements, and they are accrued on the `Cli` so that `execute` can report them all in
  // one friendly message. Inside `execute`, where the guard has already run, a missing flag
  // raises a `MissingFlagError` immediately instead, for the backstop to handle.
  transparent inline def require()
    ( using cli:           Cli,
            interpreter:   Interpreter,
            interpretable: Topic is Interpretable,
            suggestions:   (? <: Topic) is Discoverable = Discoverable.noSuggestions )
  :   Requisite[Topic] | Topic =

    cli.register(this, suggestions, interpretable.operandName)
    val value = cli.parameter[Topic](this)
    cli.demand(this, value.present)

    summonFrom:
      case _: Effectful =>
        // Unchecked deliberately: the failure is a user error, reported by the backstop, not a
        // condition for the application to handle.
        import scala.unsafeExceptions.canThrowAny
        import fulminate.errorDiagnostics.emptyDiagnostics
        value.or(throw MissingFlagError(this))

      case _ =>
        Requisite(this, value)


  // Validating a flag checks both its presence and its format: the operand arguments are
  // interpreted with the `Topic`'s `Interpretable` — the typeclass specialized for command-line
  // parameters — summoned at the expansion site as a context function over the tactic its
  // decoding may raise through, so that no `Tactic` is needed at the call site; interpretation
  // runs against a tactic conjured for it alone. (The summoned context function must be applied
  // within a single expression each time — capture checking cannot name the instance's fresh
  // capture in any storable type — hence the two separate summons below.) Like `require`,
  // failures in the pure section are accrued — as missing or as faults, with the interpreter's
  // own explanation where it raises one — and reported together by `execute`; inside `execute`,
  // a missing flag raises `MissingFlagError` and a malformed one `InvalidFlagError`,
  // immediately.
  transparent inline def validate()
    ( using cli:         Cli,
            interpreter: Interpreter,
            suggestions: (? <: Topic) is Discoverable = Discoverable.noSuggestions )
  :   Requisite[Topic] | Topic =

    given Diagnostics = Diagnostics.omit

    // Reading the operand name cannot raise, so the deferred tactic is conjured trivially; the
    // union in `Optional[Optional[Text]]` flattens to `Optional[Text]`.
    val operandName: Optional[Text] =
      safely[Hazard]:
        summonInline
         [(tactic: Tactic[Hazard]^) ?=> (Topic is Interpretable)^{tactic, caps.any}]
        . operandName

    cli.register(this, suggestions, operandName)

    val located = cli.locate(this)

    // A present flag whose interpretation yields no value without raising an error gets a
    // generic explanation, distinguishing an operand which was never given from one which
    // could not be interpreted.
    val bland =
      if located.or(Nil).nil then t"a value is required but none was given"
      else t"the value is not valid"

    // The plain conditionals below (rather than `let`/`or` chains) are deliberate: an `or`
    // default which touches the `cli` would be a by-name closure capturing a capability, which
    // `Optional#or` does not admit.
    summonFrom:
      case _: Effectful =>
        // Unchecked deliberately: the failure is a user error, reported by the backstop, not a
        // condition for the application to handle.
        import scala.unsafeExceptions.canThrowAny

        if located.absent then throw MissingFlagError(this)
        else
          attempt[Hazard]:
            summonInline
             [(tactic: Tactic[Hazard]^) ?=> (Topic is Interpretable)^{tactic, caps.any}]
            . interpret(located.or(Nil))
          . match
              case Attempt.Success(value) => value.or(throw InvalidFlagError(this, bland))
              case Attempt.Failure(error) =>
                throw InvalidFlagError(this, Error(error).message.text)

      case _ =>
        if located.absent then
          cli.demand(this, false)
          Requisite[Topic](this, Unset)
        else
          cli.present(this)
          cli.demand(this, true)

          attempt[Hazard]:
            summonInline
             [(tactic: Tactic[Hazard]^) ?=> (Topic is Interpretable)^{tactic, caps.any}]
            . interpret(located.or(Nil))
          . match
              case Attempt.Success(value) =>
                if value.absent then cli.fault(this, bland)
                Requisite[Topic](this, value)

              case Attempt.Failure(error) =>
                cli.fault(this, Error(error).message.text)
                Requisite[Topic](this, Unset)


  transparent inline def select(options: Iterable[Topic])
    ( using cli: Cli, interpreter: Interpreter, suggestible: Topic is Suggestible )
  :   Prospective[Topic] | Optional[Topic] =

    val mapping: Map[Text, Topic] =
      (options.map { option => (suggestible.suggest(option).text, option) }).to(Map)

    given interpretable: Topic is Interpretable =
      case List(value) => mapping(value())
      case _           => Unset

    // Marked as operand values, not subcommands: they are candidates for this flag's operand, so
    // the help tree must not descend into them (see `Suggestion.operand`).
    given suggestions: Topic is Discoverable =
      (_, _) => options.map(suggestible.suggest(_).copy(operand = true)).to(List)

    this()

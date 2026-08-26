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
package ambience

import scala.caps

import anticipation.*
import gossamer.*
import vacuous.*

object Configurator:
  // The application's identity in configuration namespaces, declared once as a given: it
  // provides the `MYAPP_` prefix for environment variables and the `myapp.` prefix for system
  // properties.
  case class Prefix(name: Text)

  def environment(using prefix: Prefix, environment: Environment): Configurator =
    name => environment.variable(t"${prefix.name.upper}_${name.uncamel.snake.upper}")

  def properties(using prefix: Prefix, system: System): Configurator =
    name => system(t"${prefix.name.lower}.${name.uncamel.dotted}")

  // The standard cascade: system properties take priority over environment variables, with the
  // command-line (which always takes priority over both) handled structurally by whichever type
  // consults this configurator. Declaring a `Prefix` given is what activates it; a user-defined
  // `Configurator` given in lexical scope overrides it.
  given default(using Prefix, Environment, System): Configurator =
    properties ++ environment

// An open interface over a single source of application configuration: environment variables,
// system properties, a configuration file in any format, or any other provider. A configurator
// is keyed by a setting's canonical camelCase name (e.g. `logLevel`), and each source owns its
// own mapping into its native namespace (`MYAPP_LOG_LEVEL`, `myapp.log.level`, and so on).
// Configurators compose with `++`; composition order is priority order.
//
// A CAPABILITY class: a configurator reaches outside the program (the environment, system
// properties, configuration files), and its implementations capture the capabilities they read
// through, so the unannotated type `Configurator` is tracked wherever it appears — users write
// `using Configurator`, not `using Configurator^`. SHARED, not exclusive: a configurator is
// read-only and freely aliasable.
trait Configurator extends caps.SharedCapability:
  def read(name: Text): Optional[Text]

  // The composed configurator is a FRESH capability (instantiating a capability class mints
  // one), so the result is the unannotated — implicitly tracked — type rather than the
  // previous hand-written `^{this, that}`.
  @targetName("compose")
  infix def ++ (that: Configurator): Configurator =
    name => read(name).or(that.read(name))

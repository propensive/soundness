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
package anticipation

import scala.language.experimental.into

object Log:
  // `message` is by-name: with no sink in scope (or none accepting the level), `Loggable.fanOut`
  // never forces it, so the logged value is not even constructed.
  def fine[loggable](message: => loggable)(using loggable0: (loggable is Loggable)^): Unit =
    loggable0.log(Level.Fine, System.currentTimeMillis, message)

  def info[loggable](message: => loggable)(using loggable0: (loggable is Loggable)^): Unit =
    loggable0.log(Level.Info, System.currentTimeMillis, message)

  def warn[loggable](message: => loggable)(using loggable0: (loggable is Loggable)^): Unit =
    loggable0.log(Level.Warn, System.currentTimeMillis, message)

  def fail[loggable](message: => loggable)(using loggable0: (loggable is Loggable)^): Unit =
    loggable0.log(Level.Fail, System.currentTimeMillis, message)

  // A standard, growable vocabulary describing the *nature* of a log event. An event type (or, more
  // usually, an individual enum case) mixes in one or more marker traits; a `Logger` may then be
  // restricted to record only certain categories, a filter which crosscuts libraries. Each marker
  // trait is paired with a companion `Category` selector value carrying the trait's runtime
  // `Class`, so a logger can test each event by type at runtime (the log call site only knows the
  // event type, so the concrete case's category is not statically available).
  abstract class Category(val reference: Class[?])

  object Memory extends Category(classOf[Memory])
  transparent trait Memory

  object Cpu extends Category(classOf[Cpu])
  transparent trait Cpu

  object Threading extends Category(classOf[Threading])
  transparent trait Threading

  object Process extends Category(classOf[Process])
  transparent trait Process

  object Filesystem extends Category(classOf[Filesystem])
  transparent trait Filesystem

  object Disk extends Category(classOf[Disk])
  transparent trait Disk

  object Network extends Category(classOf[Network])
  transparent trait Network

  object Database extends Category(classOf[Database])
  transparent trait Database

  object Cache extends Category(classOf[Cache])
  transparent trait Cache

  object Serialization extends Category(classOf[Serialization])
  transparent trait Serialization

  object Crypto extends Category(classOf[Crypto])
  transparent trait Crypto

  object Auth extends Category(classOf[Auth])
  transparent trait Auth

  object Configuration extends Category(classOf[Configuration])
  transparent trait Configuration

  object Dependency extends Category(classOf[Dependency])
  transparent trait Dependency

  object Scheduler extends Category(classOf[Scheduler])
  transparent trait Scheduler

  object Time extends Category(classOf[Time])
  transparent trait Time

  object Ui extends Category(classOf[Ui])
  transparent trait Ui

  object Protocol extends Category(classOf[Protocol])
  transparent trait Protocol

  object Compiler extends Category(classOf[Compiler])
  transparent trait Compiler

  object Runtime extends Category(classOf[Runtime])
  transparent trait Runtime

  object Gc extends Category(classOf[Gc])
  transparent trait Gc

  object Resource extends Category(classOf[Resource])
  transparent trait Resource

  object Security extends Category(classOf[Security])
  transparent trait Security

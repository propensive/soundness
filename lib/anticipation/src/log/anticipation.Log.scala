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
package anticipation

import language.experimental.into

object Log:
  // `message` is by-name: with no sink in scope (or none accepting the level), `Loggable.fanOut`
  // never forces it, so the logged value is not even constructed.
  def fine[loggable: Loggable](message: => loggable): Unit =
    loggable.log(Level.Fine, System.currentTimeMillis, message)

  def info[loggable: Loggable](message: => loggable): Unit =
    loggable.log(Level.Info, System.currentTimeMillis, message)

  def warn[loggable: Loggable](message: => loggable): Unit =
    loggable.log(Level.Warn, System.currentTimeMillis, message)

  def fail[loggable: Loggable](message: => loggable): Unit =
    loggable.log(Level.Fail, System.currentTimeMillis, message)

  // A standard, growable vocabulary describing the *nature* of a log event. An event type (or, more
  // usually, an individual enum case) mixes in one or more marker traits; a `Logger` may then be
  // restricted to record only certain categories, a filter which crosscuts libraries. Each marker
  // trait is paired with a companion `Category` selector value carrying the trait's runtime
  // `Class`, so a logger can test each event by type at runtime (the log call site only knows the
  // event type, so the concrete case's category is not statically available).
  abstract class Category(val reference: Class[?])

  object Memory extends Category(classOf[Memory])
  trait Memory

  object Cpu extends Category(classOf[Cpu])
  trait Cpu

  object Threading extends Category(classOf[Threading])
  trait Threading

  object Process extends Category(classOf[Process])
  trait Process

  object Filesystem extends Category(classOf[Filesystem])
  trait Filesystem

  object Disk extends Category(classOf[Disk])
  trait Disk

  object Network extends Category(classOf[Network])
  trait Network

  object Database extends Category(classOf[Database])
  trait Database

  object Cache extends Category(classOf[Cache])
  trait Cache

  object Serialization extends Category(classOf[Serialization])
  trait Serialization

  object Crypto extends Category(classOf[Crypto])
  trait Crypto

  object Auth extends Category(classOf[Auth])
  trait Auth

  object Configuration extends Category(classOf[Configuration])
  trait Configuration

  object Dependency extends Category(classOf[Dependency])
  trait Dependency

  object Scheduler extends Category(classOf[Scheduler])
  trait Scheduler

  object Time extends Category(classOf[Time])
  trait Time

  object Ui extends Category(classOf[Ui])
  trait Ui

  object Protocol extends Category(classOf[Protocol])
  trait Protocol

  object Compiler extends Category(classOf[Compiler])
  trait Compiler

  object Runtime extends Category(classOf[Runtime])
  trait Runtime

  object Gc extends Category(classOf[Gc])
  trait Gc

  object Resource extends Category(classOf[Resource])
  trait Resource

  object Security extends Category(classOf[Security])
  trait Security

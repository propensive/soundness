/*
    Exoskeleton, version 0.4.0. Copyright 2017-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import rudiments.*
import gossamer.*

private[exoskeleton] type Label = String & Singleton

case class Shell(apis: List[Api]):
  def completion: Boolean = false
  def append(api: List[Api]): Shell = api.foldLeft(this)(_.append(_))
  def append(api: Api): Shell = api.transform(this)

case class Completion()

enum Return:
  case Ok
  case Fail(code: Int = 1)
  case Completions(completions: List[Completion])

class CliContext[-A <: Api & Singleton](api: List[Api])

trait Api:
  type Return
  def transform(shell: Shell): Shell = shell.copy(apis = this :: shell.apis)
  def apply[A <: Api & Singleton]()(using Shell, CliContext[? >: this.type]): Return

object ParamShow:
  given [T: Show]: ParamShow[T] = value => List(summon[Show[T]].show(value))

trait ParamShow[T]:
  def show(value: T): List[Text]

object ParamParser:
  given [T](using ext: Unapply[Text, T]): ParamParser[T] = _.headOption.flatMap(ext.unapply(_))
  given ParamParser[List[Text]] = Some(_)

trait ParamParser[T]:
  def read(value: List[Text]): Option[T]

object Arg:
  def apply[T: ParamParser: ParamShow](longName: Label, secret: Boolean = false): Arg[longName.type, T] =
    new Arg[longName.type, T](longName, secret, summon[ParamParser[T]], summon[ParamShow[T]])

class Arg[A <: Label, T](longName: String, secret: Boolean = false, parser: ParamParser[T], show: ParamShow[T])
extends Api:
  type Return = T
  def apply[A <: Api & Singleton]()(using Shell, CliContext[? >: this.type]): T =
    throw Mistake("Not implemented")

object Flag:
  def apply[T](longName: Label, secret: Boolean = false): Flag[longName.type] =
    new Flag[longName.type](longName, secret)

class Flag[A <: Label](longName: String, secret: Boolean = false) extends Api:
  type Return = Boolean
  def apply[A <: Api & Singleton]()(using Shell, CliContext[? >: this.type]): Boolean =
    throw Mistake("Not implemented")

object Positional:
  def apply[T: ParamParser: ParamShow](name: Label, choices: List[T]): Positional[name.type, T] =
    new Positional[name.type, T](name, choices, summon[ParamParser[T]], summon[ParamShow[T]])

class Positional[A <: Label, T](var name: Label, choices: List[T], parser: ParamParser[T],
    show: ParamShow[T])

trait Fix[A]
object Fix:
  given [A]: Fix[A] = new Fix[A] {}

class CliApi[A <: Api & Singleton](shell: Shell, apis: Api*):
  def apply[T](fn: Shell ?=> CliContext[A] ?=> T): T =
    fn(using shell)(using CliContext[A](apis.to(List)))

def proffer[A <: Api & Singleton](api: A*)(using Fix[A], Shell): CliApi[A] =
  CliApi(summon[Shell], api*)

class CanExecute()

def execute(fn: CanExecute ?=> Return)(using Shell): Return =
  if summon[Shell].completion then Return.Completions(Nil)
  else fn(using new CanExecute())

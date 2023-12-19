/*
    Inimitable, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package inimitable

import anticipation.*
import vacuous.*
import rudiments.*
import perforate.*
import fulminate.*

import scala.quoted.*

import java.util as ju

import language.experimental.captureChecking

case class UuidError(badUuid: Text) extends Error(msg"$badUuid is not a valid UUID")

object Uuid:
  def parse(text: Text)(using Raises[UuidError]): Uuid =
    try
      val uuid = ju.UUID.fromString(text.s).nn
      Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)
    catch case _: Exception => raise(UuidError(text))(Uuid(0L, 0L))

  def unapply(text: Text): Option[Uuid] =
    try Some:
      val uuid = ju.UUID.fromString(text.s).nn
      Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)
    catch case err: Exception => None

  def apply(): Uuid =
    val uuid = ju.UUID.randomUUID().nn
    Uuid(uuid.getMostSignificantBits, uuid.getLeastSignificantBits)

case class Uuid(msb: Long, lsb: Long):
  def java: ju.UUID = ju.UUID(msb, lsb)
  
  def bytes: Bytes =
    (Bytes(msb).mutable(using Unsafe) ++ Bytes(lsb).mutable(using Unsafe)).immutable(using Unsafe)

  def text: Text = this.java.toString.tt
  
  @targetName("invert")
  def unary_~ = Uuid(~msb, ~lsb)
  
  @targetName("xor")
  def ^(right: Uuid): Uuid = Uuid(msb ^ right.msb, lsb ^ right.lsb)

object Inimitable:
  def uuid(expr: Expr[StringContext])(using Quotes): Expr[Uuid] =
    val text = expr.valueOrAbort.parts.head.tt
    val uuid = failCompilation(Uuid.parse(text))
    
    '{Uuid(${Expr(uuid.msb)}, ${Expr(uuid.lsb)})}
    
extension (inline context: StringContext)
  inline def uuid(): Uuid = ${Inimitable.uuid('context)}

lazy val jvmInstanceId: Uuid = Uuid()

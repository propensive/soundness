/*
    Rudiments, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import scala.collection as sc

object Bijection:
  def apply[KeyType, ValueType](map: Map[KeyType, ValueType]): Bijection[KeyType, ValueType] =
    Bijection(map, map.map(_.swap).to(Map))

case class Bijection[KeyType, ValueType]
   (map: Map[KeyType, ValueType], transposition: Map[ValueType, KeyType])
extends Iterable[(KeyType, ValueType)], sc.Map[KeyType, ValueType]:
  private inline def bijection: this.type = this
  def iterator: Iterator[(KeyType, ValueType)] = map.iterator

  infix def - (key: KeyType): Bijection[KeyType, ValueType] =
    Bijection(map - key, transposition - map(key))

  infix def - (key1: KeyType, key2: KeyType, keys: Seq[KeyType]): Bijection[KeyType, ValueType] =
    Bijection(map - key1 - key2 -- keys, transposition - map(key1) - map(key2) -- keys.map(map(_)))

  def get(key: KeyType): Option[ValueType] = map.get(key)
  def flip: Bijection[ValueType, KeyType] = new Bijection(transposition, map):
    override def flip: Bijection[KeyType, ValueType] = bijection

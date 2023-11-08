/*
    Camouflage, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package camouflage

import rudiments.*

import scala.collection.mutable as scm

//import scala.language.experimental.captureChecking

import java.util.concurrent.atomic as juca

class LruCache[KeyType, ValueType](maxSize: Int):

  override def toString(): String = s"${values} / ${ids} / $keys"
  
  private val counter: juca.AtomicInteger = juca.AtomicInteger(0)
  private val values: scm.HashMap[Int, ValueType] = scm.HashMap()
  private val ids: scm.HashMap[KeyType, Int] = scm.HashMap()
  private val keys: scm.TreeMap[Int, KeyType] = scm.TreeMap()

  protected def evict(): Unit = while values.size > maxSize do
    val id = keys.firstKey
    val key = keys(id)
    ids -= key
    keys -= id
    values -= id
  
  protected def touch(oldId: Int, newId: Int, key: KeyType, value: ValueType): Unit =
    keys(newId) = key
    values(newId) = value
    ids(key) = newId
    keys -= oldId
    values -= oldId

  def contains(key: KeyType): Boolean = ids.contains(key)

  def remove(key: KeyType): Unit =
    ids.getOrElse(key, Unset).mm: id =>
      values -= id
      ids -= key
      keys -= id

  def apply(key: KeyType)(value: => ValueType): ValueType =
    val newId = counter.getAndIncrement()
    
    ids.getOrElse(key, Unset).mm: oldId =>
      values(oldId).tap(touch(oldId, newId, key, _))
    .or:
      value.tap: value =>
        values(newId) = value
        ids(key) = newId
        keys(newId) = key
        evict()
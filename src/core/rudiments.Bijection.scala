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

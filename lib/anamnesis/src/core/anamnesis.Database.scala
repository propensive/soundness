package anamnesis

import scala.collection.mutable as scm

import prepositional.*
import rudiments.*
import vacuous.*

object Database:
  erased trait Relation[LeftType, RightType]

  inline def apply[RelationsType <: Tuple]
  :     Database of RelationsType =

    new Database(valueOf[Tuple.Size[RelationsType]]):
      type Subject = RelationsType

class Database(size: Int):
  type Subject <: Tuple

  private val relations: Array[Map[Any, Set[Any]]] = Array()
  private val corelations: Array[Map[Any, Any]] = Array()

  // private def relation[LeftType, RightType]: Map[LeftType, Set[RightType]] =
  //   Tuple.IndexOf[LeftType -< RightType]
  //   val join = tag[LeftType, RightType]

  //   relations.at(join).or:
  //     roots(summon[ClassTag[LeftType]]) =
  //       roots.at(summon[ClassTag[LeftType]]).or(Set()) + summon[ClassTag[RightType]]

  //     relations(join) = Map()

  //     Map()

  //   . asInstanceOf[Map[LeftType, Set[RightType]]]

  // private def corelation[LeftType: ClassTag, RightType: ClassTag]: Map[RightType, LeftType] =
  //   val join = tag[LeftType, RightType]

  //   corelations.at(join).or:
  //     corelations(join) = Map()
  //     Map()

  //   . asInstanceOf[Map[RightType, LeftType]]

  // def insert[LeftType: ClassTag, RightType: ClassTag](left: LeftType, right: RightType)
  // :     Unit =

  //   val join = tag[LeftType, RightType]

  //   var r1: Map[LeftType, Set[RightType]] = relation[LeftType, RightType]
  //   var r2: Map[RightType, LeftType] = corelation[LeftType, RightType]

  //   r1 = r1.updated(left, r1.at(left).or(Set()) + right)

  //   r2.at(right).let: left0 =>
  //     r1 = r1.updated(left0, r1(left0) - right)

  //   r2 = r2.updated(right, left)

  //   relations(join) = r1
  //   corelations(join) = r2

  // def select[LeftType: ClassTag, RightType: ClassTag](left: LeftType)
  // :     Set[RightType] =

  //   relation[LeftType, RightType].at(left).or(Set())

  // def select[LeftType: ClassTag](): Set[LeftType] =
  //   values.at(summon[ClassTag[LeftType]]).or(Set()).asInstanceOf[Set[LeftType]]

  // def delete[LeftType: ClassTag, RightType: ClassTag](left: LeftType, right: RightType): Unit =
  //   val join = tag[LeftType, RightType]
  //   val r1 = relation[LeftType, RightType]
  //   relations(join) = r1.updated(left, r1(left) - right)

  // def delete[LeftType: ClassTag](left: LeftType): Unit =
  //   val leftTag = summon[ClassTag[LeftType]]
  //   values(leftTag) = values(leftTag).remove(left)

  //   roots.at(summon[ClassTag[LeftType]]).or(Set()).each: root =>
  //     val join = (summon[ClassTag[LeftType]], root)
  //     relations.at(join).let:
  //       case Map[LeftType, Set[?]] =>
  //         relations(join) = relations(join) - left

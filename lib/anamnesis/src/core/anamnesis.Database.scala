package anamnesis

import scala.collection.mutable as scm
import scala.reflect.*

import prepositional.*
import rudiments.*
import vacuous.*

object Database:
  erased trait Relation[LeftType, RightType]

class Database[RelationType <: Database.Relation[?, ?]]():
  private val relations: scm.HashMap[Any, Map[?, Set[?]]] = scm.HashMap()
  private val corelations: scm.HashMap[Any, Map[?, ?]] = scm.HashMap()

  private def tag[LeftType: ClassTag, RightType: ClassTag]
  :     (ClassTag[LeftType], ClassTag[RightType]) =
    (summon[ClassTag[LeftType]], summon[ClassTag[RightType]])

  private def relation[LeftType: ClassTag, RightType: ClassTag]: Map[LeftType, Set[RightType]] =
    val join = tag[LeftType, RightType]
    relations.at(join).or:
      relations(join) = Map()
      Map()

    . asInstanceOf[Map[LeftType, Set[RightType]]]

  private def corelation[LeftType: ClassTag, RightType: ClassTag]: Map[RightType, LeftType] =
    val join = tag[LeftType, RightType]

    corelations.at(join).or:
      corelations(join) = Map()
      Map()

    . asInstanceOf[Map[RightType, LeftType]]

  def insert[LeftType: ClassTag, RightType: ClassTag](left: LeftType, right: RightType)
  :     Unit =

    val join = tag[LeftType, RightType]

    var r1: Map[LeftType, Set[RightType]] = relation[LeftType, RightType]
    var r2: Map[RightType, LeftType] = corelation[LeftType, RightType]

    r1 = r1.updated(left, r1.at(left).or(Set()) + right)

    r2.at(right).let: left0 =>
      r1 = r1.updated(left0, r1(left0) - right)

    r2 = r2.updated(right, left)

    relations(join) = r1
    corelations(join) = r2

  def select[LeftType: ClassTag, RightType: ClassTag](left: LeftType)
  :     Set[RightType] =

    relation[LeftType, RightType].at(left).or(Set())

  def delete[LeftType: ClassTag, RightType: ClassTag](left: LeftType, right: RightType): Unit =
    val join = tag[LeftType, RightType]
    val r1 = relation[LeftType, RightType]
    relations(join) = r1.updated(left, r1(left) - right)

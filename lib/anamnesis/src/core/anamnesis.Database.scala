package anamnesis

import prepositional.*
import rudiments.*
import vacuous.*

object Database:
  erased trait Relation[LeftType, RightType]

  inline def apply[RelationsType <: Tuple](): Database of RelationsType =
    val size = valueOf[Tuple.Size[RelationsType]]
    new Database(size).asInstanceOf[Database of RelationsType]

class Database(size: Int):
  import Database.Relation
  type Subject <: Tuple
  type AllRelations = Tuple.Union[Subject]

  type Has[RelationType <: Relation[?, ?]] = RelationType <:< AllRelations

  private val relations: Array[Map[?, Set[?]]] = Array.fill(size)(Map())
  private val corelations: Array[Map[?, ?]] = Array.fill(size)(Map())

  private inline def collectRelations[LeftType, TodoType <: Tuple]
     (fn: [RightType] => Map[LeftType, Set[RightType]] => Unit, index: Int = 0)
  :     Unit =

    inline !![TodoType] match
      case _: EmptyTuple => ()
      case _: (head *: tail) => inline !![head] match
        case _: Relation[LeftType, right] =>
          fn[right](relations(index).asInstanceOf[Map[LeftType, Set[right]]])

        case _ =>
          collectRelations[LeftType, tail](fn, index + 1)

  inline def relate[LeftType, RightType]: Map[LeftType, Set[RightType]] =
    val relationIndex = !![Subject].indexOf[LeftType -< RightType]
    relations(relationIndex).asInstanceOf[Map[LeftType, Set[RightType]]]

  inline def corelate[LeftType, RightType]: Map[RightType, LeftType] =
    val relationIndex = !![Subject].indexOf[LeftType -< RightType]
    corelations(relationIndex).asInstanceOf[Map[RightType, LeftType]]


  inline def assign[LeftType, RightType](left: LeftType, right: RightType)
     (using (LeftType -< RightType) <:< Tuple.Union[Subject])
  :     Unit =

    val relationIndex = !![Subject].indexOf[LeftType -< RightType]
    val relation = relate[LeftType, RightType]
    val corelation = corelate[LeftType, RightType]

    val relation2 = relation.updated(left, relation.at(left).or(Set()) + right)
    val corelation2 = corelation.updated(right, left)
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2

  inline def lookup[LeftType, RightType](left: LeftType): Set[RightType] =
    relate[LeftType, RightType].at(left).or(Set())

  inline def unassign[LeftType, RightType](left: LeftType, right: RightType)
     (using (LeftType -< RightType) <:< Tuple.Union[Subject])
  :     Unit =

    val relationIndex = !![Subject].indexOf[LeftType -< RightType]
    val relation = relate[LeftType, RightType]
    val corelation = corelate[LeftType, RightType]

    val relation2 = relation.updated(left, relation.at(left).let(_ - right).or(Set()))
    val corelation2 = corelation - right
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2

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

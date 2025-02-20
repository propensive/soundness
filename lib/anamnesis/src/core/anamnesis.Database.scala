package anamnesis

import scala.collection.mutable as scm

import contingency.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

object Database:
  erased trait Relation[LeftType, RightType]

  inline def apply[RelationsType <: Tuple](): Database of RelationsType =
    val size = valueOf[Tuple.Size[RelationsType]]
    new Database(size).asInstanceOf[Database of RelationsType]

case class DbError()(using Diagnostics) extends Error(m"Database error")

object Anamnesis:
  trait Referenceable:
    type Format
    type Subject

  opaque type Ref <: Referenceable = Int & Referenceable

  object Ref:
    def apply[RefType](db: Database): Ref of RefType in db.type = db.allocate[RefType]()

export Anamnesis.Ref

class Database(size: Int):
  import Database.Relation

  private var nextId: Int = 1

  def allocate[RefType](): Ref of RefType in this.type =
    nextId.asInstanceOf[Ref of RefType in this.type].also:
      nextId += 1

  type Subject <: Tuple
  type AllRelations = Tuple.Union[Subject]

  type Has[RelationType <: Relation[?, ?]] = RelationType <:< AllRelations

  private var references: Map[Any, Ref] = Map()
  private var dereferences: Map[Ref, Any] = Map()

  private val relations: Array[Map[Ref, Set[Ref]]] = Array.fill(size)(Map())
  private val corelations: Array[Map[Ref, Ref]] = Array.fill(size)(Map())

  protected inline def relate[LeftType, RightType]: Map[Ref, Set[Ref]] =
    relations(!![Subject].indexOf[LeftType -< RightType])

  protected inline def corelate[LeftType, RightType]: Map[Ref, Ref] =
    corelations(!![Subject].indexOf[LeftType -< RightType])

  inline def store[LeftType](left: LeftType): Ref of LeftType in this.type =
    references.at(left).or:
      allocate[LeftType]().tap: ref =>
        this.synchronized:
          references = references.updated(left, ref)
          dereferences = dereferences.updated(ref, left)

    . asInstanceOf[Ref of LeftType in this.type]

  inline def assign[LeftType, RightType](left: LeftType, right: RightType)
     (using (LeftType -< RightType) <:< Tuple.Union[Subject])
  :     Unit raises DbError =

    val leftRef = references.at(left).or(abort(DbError()))
    val rightRef = references.at(right).or(abort(DbError()))

    val relationIndex = !![Subject].indexOf[LeftType -< RightType]
    val relation = relate[LeftType, RightType]
    val corelation = corelate[LeftType, RightType]

    val relation2 = relation.updated(leftRef, relation.at(leftRef).or(Set()) + rightRef)
    val corelation2 = corelation.updated(rightRef, leftRef)
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2

  inline def lookup[LeftType, RightType](left: LeftType): Set[RightType] raises DbError =
    val leftRef = references.at(left).or(abort(DbError()))
    val set: Set[Ref] = relate[LeftType, RightType].at(leftRef).or(Set())
    set.map(dereferences(_).asInstanceOf[RightType])

  inline def unassign[LeftType, RightType](left: LeftType, right: RightType)
     (using (LeftType -< RightType) <:< Tuple.Union[Subject])
  :     Unit raises DbError =

    val leftRef = references.at(left).or(abort(DbError()))
    val rightRef = references.at(right).or(abort(DbError()))

    val relationIndex = !![Subject].indexOf[LeftType -< RightType]
    val relation = relate[LeftType, RightType]
    val corelation = corelate[LeftType, RightType]

    val relation2: Map[Ref, Set[Ref]] = relation.updated(leftRef, relation.at(leftRef).let(_ - rightRef).or(Set()))
    val corelation2 = corelation - rightRef
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

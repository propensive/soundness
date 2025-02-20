package anamnesis

import contingency.*
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

  def dereference[RefType](ref: Ref of RefType): RefType = dereferences(ref).asInstanceOf[RefType]

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

  inline def ref[LeftType](left: LeftType): Ref of LeftType in this.type raises DataError =
    references.at(left).or(abort(DataError())).asInstanceOf[Ref of LeftType in this.type]

  inline def assign[LeftType, RightType]
     (left: Ref of LeftType in this.type, right: Ref of RightType in this.type)
     (using (LeftType -< RightType) <:< Tuple.Union[Subject])
  :     Unit raises DataError =

    val relationIndex = !![Subject].indexOf[LeftType -< RightType]
    val relation = relate[LeftType, RightType]
    val corelation = corelate[LeftType, RightType]

    val relation2 = relation.updated(left, relation.at(left).or(Set()) + right)
    val corelation2 = corelation.updated(right, left)
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2

  inline def lookup[LeftType, RightType](left: Ref of LeftType in this.type)
  :     Set[Ref of RightType in this.type] raises DataError =
    relate[LeftType, RightType].at(left).or(Set()).asInstanceOf[Set[Ref of RightType in this.type]]

  inline def unassign[LeftType, RightType]
     (left: Ref of LeftType in this.type, right: Ref of RightType in this.type)
     (using (LeftType -< RightType) <:< Tuple.Union[Subject])
  :     Unit raises DataError =

    val relationIndex = !![Subject].indexOf[LeftType -< RightType]
    val relation = relate[LeftType, RightType]
    val corelation = corelate[LeftType, RightType]

    val relation2: Map[Ref, Set[Ref]] = relation.updated(left, relation.at(left).let(_ - right).or(Set()))
    val corelation2 = corelation - right
    relations(relationIndex) = relation2
    corelations(relationIndex) = corelation2

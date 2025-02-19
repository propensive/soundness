package anamnesis

import scala.reflect.*

infix type -< [LeftType, RightType] = Database.Relation[LeftType, RightType]
infix type >- [LeftType, RightType] = Database.Relation[RightType, LeftType]

extension [LeftType: ClassTag](left: LeftType)
  def select[RightType: ClassTag](using db: Database[? <: LeftType -< RightType]): Set[RightType] =
    db.select(left)

  def insert[RightType: ClassTag](right: RightType)(using db: Database[? <: LeftType -< RightType])
  :     Unit =
    db.insert(left, right)

  def delete[RightType: ClassTag](right: RightType)(using db: Database[? <: LeftType -< RightType])
  :     Unit =

    db.delete(left, right)

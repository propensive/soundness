package anamnesis

import contingency.*
import prepositional.*

infix type -< [LeftType, RightType] = Database.Relation[LeftType, RightType]
infix type >- [LeftType, RightType] = Database.Relation[RightType, LeftType]

extension [LeftType](left: LeftType)

  inline def store()(using db: Database): Unit raises DbError = db.store(left)
  //inline def discard()(using db: Database): Unit = db.discard(left)

  inline def assign[RightType](right: RightType)(using db: Database)
     (using db.Has[LeftType -< RightType])
  :     Unit raises DbError =
    db.assign(left, right)

  inline def lookup[RightType](using db: Database)(using db.Has[LeftType -< RightType])
  :     Set[RightType] raises DbError =
    db.lookup[LeftType, RightType](left)

  inline def unassign[RightType](right: RightType)(using db: Database)
     (using db.Has[LeftType -< RightType])
  :     Unit raises DbError =
    db.unassign(left, right)

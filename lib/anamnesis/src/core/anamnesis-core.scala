package anamnesis

import contingency.*
import prepositional.*

infix type -< [LeftType, RightType] = Database.Relation[LeftType, RightType]
infix type >- [LeftType, RightType] = Database.Relation[RightType, LeftType]

extension [LeftType](using db: Database)(left: Ref of LeftType in db.type)
  inline def unassign[RightType](right: Ref of RightType in db.type)
     (using db.Has[LeftType -< RightType])
  :     Unit raises DataError =
    db.unassign(left, right)

  inline def lookup[RightType](using db.Has[LeftType -< RightType])
  :     Set[Ref of RightType in db.type] raises DataError =
    db.lookup[LeftType, RightType](left)

  inline def assign[RightType](right: Ref of RightType in db.type)
     (using db.Has[LeftType -< RightType])
  :     Unit raises DataError =
      db.assign(left, right)

extension [LeftType](using db: Database)(left: LeftType)
  inline def store(): Ref of LeftType in db.type = db.store(left)
  inline def ref(): Ref of LeftType in db.type raises DataError = db.ref(left)

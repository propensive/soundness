package anamnesis

infix type -< [LeftType, RightType] = Database.Relation[LeftType, RightType]
infix type >- [LeftType, RightType] = Database.Relation[RightType, LeftType]

extension [LeftType](left: LeftType)

  inline def assign[RightType](right: RightType)(using db: Database)
     (using db.Has[LeftType -< RightType])
  :     Unit =
    db.assign(left, right)

  inline def lookup[RightType](using db: Database)(using db.Has[LeftType -< RightType])
  :     Set[RightType] =
    db.lookup[LeftType, RightType](left)

  inline def unassign[RightType](right: RightType)(using db: Database)
     (using db.Has[LeftType -< RightType])
  :     Unit =
    db.unassign(left, right)

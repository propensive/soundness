package anamnesis

import prepositional.*

object Anamnesis:
  trait Dereferenceable:
    type Format <: Database
    type Subject

  opaque type Ref <: Dereferenceable = Int & Dereferenceable

  object Ref:
    def apply[RefType](db: Database): Ref of RefType in db.type = db.allocate[RefType]()

  extension [RefType, DatabaseType <: Database](ref: Ref of RefType in DatabaseType)
    def apply()(using db: DatabaseType): RefType = db.dereference(ref)

export Anamnesis.Ref

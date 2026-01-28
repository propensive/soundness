package gnossienne

import scala.annotation.*

import adversaria.*
import anticipation.*
import contingency.*
import prepositional.*

object Resolvable:
  def apply[result](store: => Set[result])[operand]
       (using annotated:   result is Annotated by index,
              dereference: result is Dereferenceable to operand,
              encodable:   operand is Encodable in Text)
  : result is Resolvable by operand raises ReferenceError =

      new Resolvable:
        type Self = result
        type Operand = operand

        def field: Text = annotated.asInstanceOf[Annotated.Field].field

        def resolve(reference: Operand): Self raises ReferenceError =
          def deref(value: result): operand =
            dereference.select(value, annotated.asInstanceOf[Annotated.Field].field)

          store.find(deref(_) == reference).getOrElse:
            abort(ReferenceError(reference.encode))


trait Resolvable:
  type Self
  type Operand
  def resolve(reference: Operand): Self raises ReferenceError
  def field: Text

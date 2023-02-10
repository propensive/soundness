package polyvinyl

enum Datatype:
  case Varchar, Numeric

import Datatype.*

abstract class GeneralSchema(val types: Map[String, Datatype])
extends Schema[Datatype]:
  
  type Result[D <: Datatype] = D match
    case Varchar.type => String
    case Numeric.type => Int

object Address extends GeneralSchema(Map("houseNo" -> Numeric, "street" -> Varchar, "city" -> Varchar)):
  transparent inline def record(inline fn: String => Any): Record = ${build('fn)}

object Person extends Schema[Datatype]:
  def types: Map[String, Datatype] = Map(
    "name"    -> Varchar,
    "age"     -> Numeric
  )

  transparent inline def record(inline fn: String => Any): Record = ${build('fn)}

  type Result[D <: Datatype] = D match
    case Varchar.type => String
    case Numeric.type => Int
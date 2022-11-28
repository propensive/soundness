package cellulose

import polyvinyl.{Schema => PvSchema, *}
import gossamer.*
import rudiments.*

abstract class RecordType(schema: Schema) extends PvSchema[Arity]:
  import Arity.*

  lazy val types: Map[String, Arity] = schema.subschemas.collect:
    case Schema.Entry(key: Text, sch) => key.s -> sch.arity
  .to(Map)

  type Result[T <: Arity] = T match
    case One.type        => Text
    case Many.type       => List[Text]
    case Optional.type   => Maybe[Text]
    case AtLeastOne.type => List[Text]
    case Unique.type     => Maybe[Text]

  transparent inline def apply(indexed: Indexed): Record = record: key =>
    
    try
      val data = indexed.selectDynamic(key)
      data.head.schema.mm(_.arity).or(One) match
        case One        => data.head.children.head.key
        case Many       => data.flatMap(_.children.to(List).map(_.key).sift[Text])
        case Optional   => data.head.children.headOption.map(_.key).getOrElse(Unset)
        case AtLeastOne => data.head.children.map(_.key)
        case Unique     => data.head.children.head.key
    catch case err: MissingValueError => Unset
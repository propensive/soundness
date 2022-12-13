package cellulose

import rudiments.*
import gossamer.*
import wisteria.*

import Arity.*

import language.experimental.pureFunctions

trait Codec[T]:
  def serialize(value: T): List[IArray[Node]]
  def deserialize(value: List[Indexed]): T throws IncompatibleTypeError
  def schema: Schema

  def text[T](nodes: List[Indexed]): Text throws IncompatibleTypeError =
    nodes.headOption.getOrElse(throw IncompatibleTypeError()).children match
      case IArray(Node(Data(value, _, _, _), _)) => value
      case _                                     => throw IncompatibleTypeError()

object Codec extends Derivation[Codec]:
  
  def join[T](ctx: CaseClass[Codec, T]): Codec[T] = new Codec[T]:
    def schema: Schema =
      val entries: IArray[Schema.Entry] = ctx.params.map: param =>
        Schema.Entry(param.label.show, param.typeclass.schema)

      Struct(entries.to(List), Arity.One)
    
    def serialize(value: T): List[IArray[Node]] = List:
      ctx.params.flatMap: p =>
        val children = p.typeclass.serialize(p.deref(value)).map: children =>
          val index = children.zipWithIndex.foldLeft(Map[Text, Int]()):
            case (map, (child, idx)) => child.mm(_.id).fm(map)(map.updated(_, idx))

        p.typeclass.serialize(p.deref(value)).map: value =>
          Node(Data(p.label.show, value, Layout.empty, p.typeclass.schema))
        .filter(!_.empty)

    def deserialize(value: List[Indexed]): T throws IncompatibleTypeError = ctx.construct: param =>
      param.typeclass.deserialize(value.head.get(param.label.show))
  
  def split[T](ctx: SealedTrait[Codec, T]): Codec[T] = ???
  
  given Codec[Byte] with
    def schema = Field(Arity.One)
    def serialize(value: Byte): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[Indexed]): Byte throws IncompatibleTypeError = text(value).as[Byte]
 
  given Codec[Short] with
    def schema = Field(Arity.One)
    def serialize(value: Short): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[Indexed]): Short throws IncompatibleTypeError = text(value).as[Short]

  given Codec[Long] with
    def schema = Field(Arity.One)
    def serialize(value: Long): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[Indexed]): Long throws IncompatibleTypeError = text(value).as[Long]

  given Codec[Char] with
    def schema = Field(Arity.One)
    def serialize(value: Char): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[Indexed]): Char throws IncompatibleTypeError = text(value).as[Char]

  given Codec[Int] with
    def schema = Field(Arity.One)
    def serialize(value: Int): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[Indexed]): Int throws IncompatibleTypeError = text(value).as[Int]

  given Codec[Text] with
    def schema = Field(Arity.One)
    def serialize(value: Text): List[IArray[Node]] = List(IArray(Node(Data(value.show))))
    def deserialize(value: List[Indexed]): Text throws IncompatibleTypeError = text(value)

  given Codec[Boolean] with
    def schema = Field(Arity.One)
    def serialize(value: Boolean): List[IArray[Node]] = List(IArray(Node(Data(if value then t"yes" else t"no"))))

    def deserialize(value: List[Indexed]): Boolean throws IncompatibleTypeError = text(value) match
      case t"yes" => true
      case t"no"  => false
      case value  => throw IncompatibleTypeError()
  
  given option[T](using codec: Codec[T]): Codec[Option[T]] = new Codec[Option[T]]:
    def schema: Schema = summon[Codec[T]].schema.optional
  
    def serialize(value: Option[T]): List[IArray[Node]] = value match
      case None        => List()
      case Some(value) => codec.serialize(value)
    
    def deserialize(value: List[Indexed]): Option[T] throws IncompatibleTypeError =
      if value.isEmpty then None else Some(codec.deserialize(value))
    
  given maybe[T](using codec: Codec[T]): Codec[Maybe[T]] = new Codec[Maybe[T]]:
    def schema: Schema = summon[Codec[T]].schema.optional
  
    def serialize(value: Maybe[T]): List[IArray[Node]] = value match
      case Unset               => List()
      case value: T @unchecked => codec.serialize(value)
    
    def deserialize(value: List[Indexed]): Maybe[T] throws IncompatibleTypeError =
      if value.isEmpty then Unset else codec.deserialize(value)
    
  given list[T](using codec: => Codec[T]): Codec[List[T]] = new Codec[List[T]]:
    def schema: Schema = codec.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def serialize(value: List[T]): List[IArray[Node]] = value.map { (value: T) => codec.serialize(value).head }

    def deserialize(value: List[Indexed]): List[T] throws IncompatibleTypeError = codec.schema match
      case Field(_, validator) => value.flatMap(_.children).map: node =>
        codec.deserialize(List(Doc(node)))
      
      case struct: Struct =>
        value.map { v => codec.deserialize(List(v)) }
  
  given set[T](using codec: Codec[T]): Codec[Set[T]] = new Codec[Set[T]]:
    def schema: Schema = codec.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def serialize(value: Set[T]): List[IArray[Node]] =
      value.map { (value: T) => codec.serialize(value).head }.to(List)

    def deserialize(value: List[Indexed]): Set[T] throws IncompatibleTypeError = codec.schema match
      case Field(_, validator) =>
        value.flatMap(_.children).map: node =>
          codec.deserialize(List(Doc(node)))
        .to(Set)
      
      case struct: Struct =>
        value.map { v => codec.deserialize(List(v)) }.to(Set)
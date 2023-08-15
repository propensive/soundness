package anticipation

trait Transport[TransportType]:
  type Serializer[DataType]
  type Deserializer[DataType]

package parasite

enum Perseverance[+ValueType]:
  case Persevere
  case Surrender
  case Prevail(value: ValueType)

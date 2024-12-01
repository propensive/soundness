package aviation

object Base24:
  def apply(int: Int): Base24 =
    ((((int%24) + 24)%24): @unchecked) match
      case value: Base24 => value

  def unapply(value: Int): Option[Base24] =
    if value < 0 || value > 23 then None else Some(value.asInstanceOf[Base24])

type Base24 = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 |
    19 | 20 | 21 | 22 | 23

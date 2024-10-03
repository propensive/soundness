package denominative

object Zerary:

  final class Match(val int: Int) extends AnyVal:
    def isEmpty: false = false
    def get: Ordinal = Ordinal.zerary(int)

  inline def unapply(int: Int): Match = Match(int)
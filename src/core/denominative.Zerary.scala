package denominative

object Zerary:

  final class Match(int: Int):
    def isEmpty: false = false
    def get: Ordinal = Ordinal.zerary(int)

  inline def unapply(int: Int): Match = Match(int)
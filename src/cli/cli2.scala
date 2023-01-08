package probably2

import rudiments.*
import gossamer.*
import escapade.*

enum AnsiItem:
  case Tabular()

trait Suite(val name: Text):
  def run(using Runner[?]): Unit
  
  final def main(args: IArray[Text]): Unit =
    val runner = Runner[Any]()
    //val report: Maybe[Report] = try run(using runner) catch case error: Throwable => Unset
    
package austronesian

import language.dynamics

import anticipation.*
import hellenism.*

class Proxy(name: Text, singleton: Boolean) extends Dynamic:
  transparent inline def applyDynamic(method: String)(inline arguments: Any*)
     (using classloader: Classloader)
  :     Any =
    ${Austronesian2.proxy('name, 'method, 'arguments, 'classloader, 'singleton)}

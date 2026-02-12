package frontier

import scala.quoted.*

import anticipation.*
import proscenium.*

inline def doExplain[target]: target = ${Frontier.explain[target]}

package missingContext:
  transparent inline given explain: [any] => any = doExplain[any]

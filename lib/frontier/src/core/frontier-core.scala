package frontier

import scala.quoted.*

import anticipation.*
import proscenium.*

inline def doExplain[target]: target = ${Frontier.explain[target]}

package context:
  transparent inline given explainMissingContext: [any] => any = doExplain[any]

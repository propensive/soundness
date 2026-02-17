package frontier

import scala.quoted.*

import anticipation.*
import proscenium.*


package context:
  transparent inline given explainMissingContext: [any] => any = Frontier.explanation[any]

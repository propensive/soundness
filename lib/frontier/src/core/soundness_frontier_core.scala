package soundness

import frontier.*

package context:
  transparent inline given explainMissingContext: [any] => any = Frontier.explanation[any]

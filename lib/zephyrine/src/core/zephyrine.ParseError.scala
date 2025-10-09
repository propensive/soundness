package zephyrine

import contingency.*
import fulminate.*

case class ParseError(format: Format, position: format.Position, issue: format.Issue)
            (using Diagnostics)
extends Error(m"the ${format.name} was not valid at ${position.describe} because ${issue.describe}")

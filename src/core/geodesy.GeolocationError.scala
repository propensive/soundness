package geodesy

import fulminate.*

object GeolocationError:
  enum Reason:
    case MissingEquals, MultipleEquals, BadScheme, ExpectedSemicolon, UnexpectedSuffix,
        ExpectedCoordinates, BadUncertainty

  given Reason is Communicable =
    case Reason.MissingEquals       => m"the parameter does not contain an `=`"
    case Reason.MultipleEquals      => m"the parameter contains more than one `=`"
    case Reason.BadScheme           => m"the value does not begin with the `geo:` URI scheme"
    case Reason.ExpectedSemicolon   => m"a `;` was expected after the altitude value"
    case Reason.UnexpectedSuffix    => m"a `,` or `;` was expected"
    case Reason.ExpectedCoordinates => m"latitude and longitude coordinates were expected"
    case Reason.BadUncertainty      => m"the `uncertainty` parameter vas not a valid number"

case class GeolocationError(reason: GeolocationError.Reason)(using Diagnostics)
extends Error(m"The geo URI is not in the correct format because $reason")
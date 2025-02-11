/*
    Geodesy, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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
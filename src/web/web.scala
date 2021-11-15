/*
    Probably, version 0.18.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

import probably._
import honeycomb._, html5._
import gastronomy._

package object probably {
  implicit class ReportExtensions(report: Report) {
    def html: Element[Flow] = table(
      thead(tr(th("ID"), th("Name"), th("Count"), th("Min"), th("Avg"), th("Max"), th("Debug"))),
      tbody(
        report.results.map { summary =>
          tr(
            td(summary.id.value.show),
            td(summary.name.show),
            td(summary.count.show),
            td(summary.min.show),
            td(summary.avg.show),
            td(summary.max.show),
            td(summary.outcome.debug.show)
          )
        }
      )
    )
  }
}
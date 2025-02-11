/*
    Probably, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import ambience.*
import turbulence.*

trait Reporter[ReportType]:
  def make(): ReportType
  def fail(report: ReportType, error: Throwable, active: Set[TestId]): Unit
  def declareSuite(report: ReportType, suite: Testable): Unit
  def complete(report: ReportType): Unit

object Reporter:
  given (using Stdio, Environment): Reporter[Report] with
    def make(): Report = Report()
    def declareSuite(report: Report, suite: Testable): Unit = report.declareSuite(suite)

    def fail(report: Report, error: Throwable, active: Set[TestId]): Unit =
      report.fail(error, active)

    def complete(report: Report): Unit =
      report.complete(Coverage())

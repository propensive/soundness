/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import anticipation.*

object WorkingDirectory:
  given default(using Quickstart): WorkingDirectory =
    WorkingDirectory(Option(System.getProperty("user.dir")).map(_.nn.tt).getOrElse(Unset))

@capability
case class WorkingDirectory(text: Maybe[Text]):
  def path[PathType: SpecificPath]: Maybe[PathType] = text.mm(SpecificPath(_))

object HomeDirectory:
  given default(using Quickstart): HomeDirectory = HomeDirectory(System.getProperty("user.home").nn.tt)

@capability
case class HomeDirectory(text: Text):
  def path[PathType: SpecificPath]: PathType = SpecificPath(text)

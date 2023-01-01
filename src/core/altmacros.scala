/*
    Probably, version 0.4.0. Copyright 2017-23 Jon Pretty, Propensive OÜ.

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

import scala.quoted.*

object AltMacros:
  import scala.reflect.*
  def assert[T: Type](runner: Expr[Runner], pred: Expr[T => Boolean])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    '{ () }
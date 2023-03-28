/*
    Hieronymus, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hieronymus

import probably.*
import rudiments.*
import gossamer.*

object Tests extends Suite(t"Hieronymus tests"):
  def run(): Unit =
    test(t"Check narrow character width"):
      'a'.displayWidth
    .assert(_ == 1)

    test(t"Check Japanese character width"):
      '身'.displayWidth
    .assert(_ == 2)

    test(t"Check displayWidth of string of Japanese text"):
      t"平ぱ記動テ使村方島おゃぎむ万離ワ学つス携".displayWidth
    .assert(_ == 40)
    
    test(t"Check displayWidth of 平ぱ記動テ使村方島おゃぎむ万離ワ学つス携"):
      t"平ぱ記動テ使村方島おゃぎむ万離ワ学つス携".displayWidth
    .assert(_ == 40)
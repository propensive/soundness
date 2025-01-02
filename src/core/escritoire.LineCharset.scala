/*
    Escritoire, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escritoire

enum LineCharset:
  case Default, Rounded, Ascii

  def apply(): IArray[Char] = this match
    case Default => BoxDrawing.defaultChars
    case Rounded => BoxDrawing.roundedChars
    case Ascii   => BoxDrawing.asciiChars

  def apply
     (top: BoxLine = BoxLine.Blank,
      right: BoxLine = BoxLine.Blank,
      bottom: BoxLine = BoxLine.Blank,
      left: BoxLine = BoxLine.Blank)
          : Char =
    this()(left.ordinal + bottom.ordinal*4 + right.ordinal*16 + top.ordinal*64)

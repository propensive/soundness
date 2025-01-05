/*
    Gossamer, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package soundness

export gossamer.{Bidi, Buffer, Cuttable, Decimalizer, Interpolation, Joinable, Numerous,
    Presentational, Pue, RangeError, TextBuffer, Textual, append, construct, fill, txt, t, utf8,
    utf16, ascii, hex, text, pue, cut, pascal, camel, snake, kebab, length, populated, lower,
    upper, plain, skip, keep, capitalize, uncapitalize, tail, init, empty, chars,
    snip, reverse, contains, trim, where, upto, dropWhile, snipWhere,
    mapChars, count, metrics, pad, center, fit, uncamel, unkebab, unsnake, starts, ends,
    tr, subscript, superscript, sub, flatMap, urlEncode, urlDecode, punycode, bytes,
    sysBytes, lev, join, add, words, lines, appendln, spaced, slices, seek, search}

package decimalFormatters:
  export gossamer.decimalFormatters.java

package enumIdentification:
  export gossamer.enumIdentification.kebabCase
  export gossamer.enumIdentification.pascalCase
  export gossamer.enumIdentification.snakeCase
  export gossamer.enumIdentification.camelCase

/*
    Hypotenuse, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

export hypotenuse.{CheckOverflow, Commensurable, DivisionByZero,
    DivisionError, Orderable, OverflowError, B8, B16, B32, B64, S8, S16, S32, S64, U8, U16,
    U32, U64, F32, F64, abs, sqrt, cbrt, ceiling, floor, exponent, increment, decrement, round, scalb, signum, ulp,
    bits, rawBits, finite, infinite, nan, **, mantissa, long, int, short, octal, hex, base32, binary, %%,
    \, apply, erf, π, pi, euler, φ, goldenRatio, cos, acos, cosh, sin, asin, sinh, tan, atan, hyp, exp,
    expm1, ln, log10, log1p, <, <=, >, >=}

package arithmeticOptions:
  export hypotenuse.arithmeticOptions.{division, overflow}

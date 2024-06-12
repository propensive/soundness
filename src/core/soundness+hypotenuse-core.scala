package soundness

export hypotenuse.{CheckOverflow, CompareGreater, CompareGreaterEqual, CompareLess, CompareLessEqual, DivisionByZero,
    DivisionError, Inequality, NumericallyComparable, OverflowError, B8, B16, B32, B64, I8, I16, I32, I64, U8, U16,
    U32, U64, F32, F64, abs, sqrt, cbrt, ceiling, floor, exponent, increment, decrement, round, scalb, signum, ulp,
    bits, rawBits, finite, infinite, nan, **, mantissa, long, int, short, octal, hex, base32, binary, %%,
    \, apply, erf, π, pi, e, eulerNumber, φ, goldenRatio, cos, acos, cosh, sin, asin, sinh, tan, atan, hyp, exp,
    expm1, ln, log10, log1p, <, <=, >, >=}

package arithmeticOptions:
  export hypotenuse.arithmeticOptions.{division, overflow}

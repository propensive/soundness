When working with physical quantities, such as lengths, masses or temperatures,
it can be easy to mix up quantities with different units, especially if we
represent all quantities with `Double`s, which is often necessary for
performance.

Quantitative represents physical quantities with a generic `Quantity` type, an
opaque alias of `Double`, which statically encodes the value's units in its
type parameter. This provides all the desirable homogeneity constraints when
combining quantities, with the performance of `Double`s, and without
compromising on intuitive syntax for arithmetic operations.

Quantities can be multiplied and divided arbitrarily, with new units computed
by the compiler, and checked for consistency in additions and subtractions.



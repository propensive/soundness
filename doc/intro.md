If we work with physical quantities, such as lengths, masses or temperatures, it's too easy to mix them up,
especially if we use `Double`s to represent all of them, which we may want to do to avoid the space and
performance overhead of using different class instances for each physical value.

Quantify provides a general type, `Quantity`, for representing physical quantities with all the desirable
guarantees we need on the homogeneity of units, but with all the performance of using `Double`s, and without
compromising on intuitive syntax for arithmetic operations. Mismatched units on physical values are type errors.
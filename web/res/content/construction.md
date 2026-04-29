### Safe by Construction

Soundness makes it impossible to represent impossible states, so values are not only known to be safe by construction, but checked at compiletime. When there's not enough information available to check a value at compiletime, it's checked at runtime—but you must define what happens if it's invalid.

Values like URLs, timestamps and ports are all eagerly verified, and they're represented as literals. If they're not valid, it's a compile error. Stringly-typed values—parsed at runtime—are a thing of the past.

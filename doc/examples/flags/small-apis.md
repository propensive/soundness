# A bit set is a set like any other

A `Bitset` holds its members in bits and a `Set` holds them in a hash table, but neither has
methods of its own for membership, intersection or difference. `has`, `intersect`, `except`
and `size` are the same names on both, and only the constructor says which is which.

```scala fuel
compile
alternative A hash set
  highlight Set  a hash table
    occurrence 1
  code
      val seen = Set(1.z, 5.z, 64.z)
      val common = seen.intersect(Set(5.z, 64.z, 65.z))
      val rest = seen.except(common)
      val found: Boolean = common.has(64.z)
      val count: Int = rest.size
alternative A bit set
  highlight Bitset  one bit for each possible member
    occurrence 1
  code
      val seen = Bitset(1.z, 5.z, 64.z)
      val common = seen.intersect(Bitset(5.z, 64.z, 65.z))
      val rest = seen.except(common)
      val found: Boolean = common.has(64.z)
      val count: Int = rest.size
##
```

[Sets of integers](../../soundness/modules/flags.md#sets-of-integers) says when the bits are
the better choice, and [small APIs](../../philosophy/small-apis.md) why the names are shared.

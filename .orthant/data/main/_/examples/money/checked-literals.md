# A security identifier with the wrong checksum

An ISIN is twelve characters, and the last is a check digit. An `isin"…"` literal is validated
as the code compiles, checksum included, so a mistyped identifier never reaches a trade.

```scala fuel
compile
frame
  label A digit short
  errors show
  code
      val security = isin"GB00BH4HKS3"
frame
  label A digit wrong
  errors show
  code
      val security = isin"GB00BH4HKS38"
frame
  label A valid identifier
  highlight isin"GB00BH4HKS39"  checksum and all
  code
      val security = isin"GB00BH4HKS39"
##
```

[Identifiers](../../soundness/modules/money.md#identifiers) lists the rules, and says what an
`Isin` costs to hold.

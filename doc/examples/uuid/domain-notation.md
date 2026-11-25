# Identifiers, combined as bits

A UUID is 128 bits, and on the occasions when it is treated as bits it is written as bits
are. `^` is the exclusive-or of two identifiers and `~` the inverse of one, and each gives
another `Uuid`.

```scala fuel
compile
frame
  label Two identifiers
  code
      val viola = uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
      val sebastian = uuid"11111111-2222-3333-4444-555555555555"
frame
  label One made from both
  highlight ^  the exclusive-or of all 128 bits
  code
      val viola = uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
      val sebastian = uuid"11111111-2222-3333-4444-555555555555"

      val twins: Uuid = viola ^ sebastian
frame
  label And taken apart again
  highlight twins..sebastian  exclusive-or undoes itself
    line 5
  code
      val viola = uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
      val sebastian = uuid"11111111-2222-3333-4444-555555555555"

      val twins: Uuid = viola ^ sebastian
      val restored: Uuid = twins ^ sebastian
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(twins.text)
Out.println(restored.text)
Out.println((restored == viola).inspect)
```

[Combining](../../soundness/modules/uuid.md#combining) has the inverse, `~`.

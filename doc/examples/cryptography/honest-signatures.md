# Where a secret is kept is written down

A password must rest somewhere between uses: on the heap or off it, in the clear or encrypted.
There is no default. A secret cannot be constructed until a `Cloak` in scope says where.

```scala fuel
compile
frame
  label A secret with nowhere to rest
  errors show
  code
      val password = Password(t"hunter2")
frame
  label On the heap
  highlight heapCloak  cleartext, on the heap
  code
      import cloaks.heapCloak

      val password = Password(t"hunter2")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(password.show)
```

Even when shown, a password does not reveal itself.
[Where secrets are kept](../../soundness/modules/cryptography.md#where-secrets-are-kept)
compares the four cloaks.

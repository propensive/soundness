# A password's cleartext does not leave `uncloak`

A `Password` shows nothing of its secret. The cleartext is reached only inside `uncloak`, as a
capability which exists for the length of the block, and neither it nor a function which uses
it can be the block's result. What is computed from it can.

```scala fuel
compile
feature experimental.captureChecking
frame
  label The cleartext, handed out
  errors show
  highlight uncloak(cleartext)
  code
      import cloaks.heapCloak

      def reveal(password: Password) = password.uncloak(cleartext)
frame
  label A fact about it, computed inside
  highlight cleartext.chars.length
  code
      import cloaks.heapCloak

      def strength(password: Password): Int =
        password.uncloak(cleartext.chars.length)
##
```

[Passwords](../../soundness/modules/cryptography.md#passwords) describes how a password is kept
at rest.

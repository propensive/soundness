# A message needs a courier and a sender

`send` does not reach for a mail server of its own. It takes a `Courier`, which delivers the
message, and a `Sender`, which is who it is from, out of its context. A method which sends
mail says so, and the type of its result is whatever that courier gives back.

```scala fuel
compile
frame
  label Nothing to send with
  errors show
  code
      import codepages.utf8Codepage

      def invite(guest: EmailAddress): Unit =
        t"Pray come to the feast.".send(subject = t"A feast at the Capulets'", to = guest)
frame
  label A courier and a sender, asked of the caller
  highlight (using..Sender)
  highlight courier.Result  the receipt of whichever courier is given
  code
      import codepages.utf8Codepage

      def invite(guest: EmailAddress)(using courier: Courier, sender: Sender): courier.Result =
        t"Pray come to the feast.".send(subject = t"A feast at the Capulets'", to = guest)
##
```

[Couriers](../../soundness/modules/email.md#couriers) describes the one which is provided,
and [honest signatures](../../philosophy/honest-signatures.md) the principle.

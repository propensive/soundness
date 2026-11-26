# An invitation to nobody

The recipient of a message is an `EmailAddress`, and one written with `email"…"` is checked
by the compiler. A message to an address which could never be delivered to is not sent,
because it is not compiled.

```scala fuel
compile
frame
  label An address with no mailbox
  errors show
  code
      import codepages.utf8Codepage

      def invite()(using courier: Courier, sender: Sender): courier.Result =
        t"Pray come to the feast.".send
          ( subject = t"A feast at the Capulets'",
            to      = email"romeo.montague.example" )
frame
  label A well-formed address
  highlight email"romeo@montague.example"
  code
      import codepages.utf8Codepage

      def invite()(using courier: Courier, sender: Sender): courier.Result =
        t"Pray come to the feast.".send
          ( subject = t"A feast at the Capulets'",
            to      = email"romeo@montague.example" )
##
```

[Sending](../../soundness/modules/email.md#sending) describes `send`, and
[Network addresses](../../soundness/modules/network-addresses.md#email-addresses) the
literal.

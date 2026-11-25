# One method sends every kind of message

A line of text is sent with `send`, and so is a composed `Email` with an attachment. The
envelope is the same parameters each time, and the MIME structure follows from what the
message holds.

```scala fuel
compile
alternative Text
  highlight send
  code
      import codepages.utf8Codepage

      def invite(guest: EmailAddress)(using courier: Courier, sender: Sender): courier.Result =
        t"Pray come to the feast.".send(subject = t"A feast at the Capulets'", to = guest)
alternative An email with an attachment
  highlight attach(menu)
  highlight send
  code
      import codepages.utf8Codepage

      val menu = Asset(t"menu.csv", media"text/csv", Chain(t"course,dish\n1,marchpane".in[Data]))

      def invite(guest: EmailAddress)(using courier: Courier, sender: Sender): courier.Result =
        Email(t"Pray come to the feast.").attach(menu)
        . send(subject = t"A feast at the Capulets'", to = guest)
##
```

[Composing](../../soundness/modules/email.md#composing) has the table of content types, and
[small APIs](../../philosophy/small-apis.md) the idea.

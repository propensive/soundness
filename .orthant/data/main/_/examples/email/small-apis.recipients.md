# One guest, or the whole list

Each field of an envelope takes one address or a list of them. That is a union type, and not
a pair of methods, so there is no `sendToAll`, and a single address is never wrapped in a
list.

```scala fuel
compile
frame
  label One recipient
  highlight EmailAddress
  code
      import codepages.utf8Codepage

      def invite(guest: EmailAddress)(using courier: Courier, sender: Sender): courier.Result =
        t"Pray come to the feast.".send(subject = t"A feast at the Capulets'", to = guest)
frame
  label Many recipients
  highlight List[EmailAddress]
  code
      import codepages.utf8Codepage

      def invite(guests: List[EmailAddress])(using courier: Courier, sender: Sender)
      :   courier.Result =
        t"Pray come to the feast.".send(subject = t"A feast at the Capulets'", to = guests)
frame
  label And one in copy
  highlight cc..example"
  code
      import codepages.utf8Codepage

      def invite(guests: List[EmailAddress])(using courier: Courier, sender: Sender)
      :   courier.Result =
        t"Pray come to the feast.".send
          ( subject = t"A feast at the Capulets'",
            to      = guests,
            cc      = email"nurse@capulet.example" )
##
```

[Sending](../../soundness/modules/email.md#sending) lists the fields of the envelope.

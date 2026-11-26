# A picture in a question, named by a URL

A message is a role and a list of content, and a picture is one kind of content. Where the
picture lives is a URL, as [network addresses](../../soundness/modules/network-addresses.md#urls)
describes one: checked as the code compiles, with nothing between the two libraries to convert
it.

```scala fuel
compile
frame
  label A question in words
  code
      val question = Llm.Message(Llm.Role.User, t"Whose skull is this?")
frame
  label The same question, with a picture
  highlight url"https://example.com/yorick.png"  a checked URL, used as it is
  highlight Llm.Content.Graphic
  code
      val question = Llm.Message
        ( Llm.Role.User,
          List
            ( Llm.Content.Textual(t"Whose skull is this?"),
              Llm.Content.Graphic
                ( Llm.Content.Source.Remote(url"https://example.com/yorick.png") ) ) )
##
```

[Messages and content](../../soundness/modules/llm.md#messages-and-content) lists the kinds of
content, and [composability](../../philosophy/composability.md) the idea.

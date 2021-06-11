package honeycomb

import jovian.*
import scintillate.*

object Example:

  import attributes.strings.given
  import attributes.*, attributes.given, attributes.jovian.given, attributes.scintillate.given

  val html: HtmlDoc = HtmlDoc(Html(
    Head(
      Base(itemref = "bar"),
      Meta(itemscope = "y")
    ),
    Body(
      Table(
        Thead(style = "value")(Tr),
        Tbody(
          Tr(
            Td("Text content. The quick brown fox jumps over the lazy dog. ", Br, "text",
              P("""Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut
                   labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco
                   laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in
                   voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
                   cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."""),
              A(target = Target.Top, href = uri"http://example.com/")(List(
                //Source(lang = "hello"),
                //Source(nonce = "hello"),
                //Track(title = "bar"),
                P("hello"))
              )
            )
          ),
          Tr(
            Td("Text content",
              Form(enctype = "foo", acceptCharset = encodings.Utf8, novalidate = true)(
                Input(name = "field"),
                Input(name = "field"),
                Input(name = "field"),
                Input(name = "field"),
                Textarea(wrap = Wrap.Soft)("this is some text in a textarea field")
              )
            ),
            Td("Text content ", Em("with bold in the middle of this text, limited to a certain length"), " text")
          )
        ),
      )
    )
  ))

@main def run(): Unit =
  println(HtmlDoc.serialize(Example.html, 100))
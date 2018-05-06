package progeny

object Test {

  import html5._

  def main(args: Array[String]): Unit = println {
    html(
      head(
        template(div(p("hello"))),
      ),
      body(
        p(a("")),
        a(
          div(
            figure(
              div(p("hello")),
              figcaption(p("now hat"))
            ),
            fieldset(
              legend("hello")
            ),
            form(
              textarea()
            ),
            div(p("""
          Here is some content.
        """))
          )
        )
      )
    )
  }

}

package nidificant

import probation._

object Tests extends TestApp {

  def tests(): Unit = {
    import html5._

    val items = List(
      "/home" -> "Home",
      "/opensource" -> "Open Source",
      "/training" -> "Training",
      "/community" -> "Community",
    )

    test("do something") {
      html(
        head(
          title("Propensive"),
          meta(charset = "UTF-8"),
          link(rel = "stylesheet", href = "styles/propensive.css"),
          script(src = "script/highlight.pack.js"),
          script("hljs.initHighlightignOnLoad();"),
        ),
        body(
          header(
            ul(items.map { case (k, v) => li(a(href = k)(v)) }),
          ),
          nav(
            ul(items.map { case (k, v) => li(a(href = s"#$k")(v)) }),
          ),
          html5.main(
          ),
          footer
        )
      ).toString
    }.assert(_ == "<html/>")
  
    ()
  }

}

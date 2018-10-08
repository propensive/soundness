package vespiary

import probation._

object Tests extends TestApp {

  def tests(): Unit = {
    import html5._

    case class Area(link: String, text: String)
    case class Section(link: String, text: String)
    
    val menu = List(
      Area("/home", "Home"),
      Area("/opensource", "Open Source"),
      Area("/training", "Training"),
      Area("/community", "Community"),
    )
    
    val overview = List(
      Section("part1", "Part 1"),
      Section("part2", "Part 2"),
      Section("part3", "Part 3"),
      Section("part4", "Part 4"),
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
            ul(menu.map { area => li(a(href = area.link)(area.text)) }),
          ),
          nav(
            ul(overview.map { section => li(a(href = s"#${section.link}")(section.text)) }),
          ),
          html5.main(
            img(id = "logo",
                src = "images/magnolia_logo.png",
                style = "shape-outside: url('images/magnolia_logo.png')"),
            h1("Magnolia"),
            p(
              "Here is some content, ",
              b("in bold"),
            ),
            h2("Subheading"),
          ),
          footer(
            "© Copyright 2018 Propensive Ltd. All rights reserved. Registered in England № 6873445. VAT № GB 974 3993 62.",
          ),
        ),
      ).toString
    }.assert(_ == "<html/>")

    ()
  }

}

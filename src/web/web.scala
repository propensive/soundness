import probably._
import honeycomb._, html5._
import gastronomy._

package object probably {
  implicit class ReportExtensions(report: Report) {
    def html: Element[Flow] = table(
      thead(
        tr(
          th("ID"),
          th("Name"),
          th("Count"),
          th("Min"),
          th("Avg"),
          th("Max"),
          th("Debug")
        )
      ),
      tbody(
        report.results.map { summary =>
          tr(
            td(summary.name.digest[Sha256].encoded[Hex].take(6).toLowerCase),
            td(summary.name),
            td(summary.count.toString),
            td(summary.min.toString),
            td(summary.avg.toString),
            td(summary.max.toString),
            td(summary.result.debug)
          )
        }
      )
    )
  }
}
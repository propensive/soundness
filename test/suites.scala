package niveau

import probably.*

object Tests extends Suite("Niveau tests"):
  def run(using Runner): Unit =
    suite("Xylophone")(xylophone.Tests.run)
    suite("Iridescence")(iridescence.Tests.run)
    suite("Kaleidoscope")(kaleidoscope.Tests.run)
    suite("Guillotine")(guillotine.Tests.run)
    suite("Euphemism")(euphemism.Tests.run)
    suite("Gastronomy")(gastronomy.Tests.run)

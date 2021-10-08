package niveau

import probably.*

object Tests extends Suite("Niveau tests"):
  def run(using Runner): Unit =
    suite(adversaria.Tests)
    suite(acyclicity.Tests)
    suite(caesura.Tests)
    suite(cataract.Tests)
    suite(contextual.Tests)
    suite(cosmopolite.Tests)
    suite(escapade.Tests)
    suite(escritoire.Tests)
    suite(euphemism.Tests)
    suite(exoskeleton.Tests)
    suite(gastronomy.Tests)
    suite(gesticulate.Tests)
    suite(gossamer.Tests)
    suite(guillotine.Tests)
    suite(honeycomb.Tests)
    suite(iridescence.Tests)
    suite(jovian.Tests)
    suite(kaleidoscope.Tests)
    suite(probably.Tests)
    suite(punctuation.Tests)
    suite(rivulet.Tests)
    suite(rudiments.Tests)
    suite(scintillate.Tests)
    suite(slalom.Tests)
    //suite(tarantula.Tests)
    suite(wisteria.Tests)
    suite(xylophone.Tests)
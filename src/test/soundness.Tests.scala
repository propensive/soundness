                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package soundness

object Tests extends Suite(m"Soundness tests"):
  def run(): Unit =
    abacist.Tests()
    acyclicity.Tests()
    adversaria.Tests()
    ambience.Tests()
    anamnesis.Tests()
    anthology.Tests()
    anticipation.Tests()
    aperture.Tests()
    apoplexy.Tests()
    austronesian.Tests()
    aviation.Tests()
    baroque.Tests()
    beneficence.Tests()
    bitumen.Tests()
    breviloquence.Tests()
    burdock.Tests()
    cacophony.Tests()
    caduceus.Tests()
    caesura.Tests()
    camouflage.Tests()
    capricious.Tests()
    cardinality.Tests()
    cataclysm.Tests()
    charisma.Tests()
    chiaroscuro.Tests()
    coaxial.Tests()
    _root_.contextual.Tests()
    contingency.Tests()
    telekinesis.Http2Tests()
    //cosmopolite.Tests()
    degustation.Tests()
    dendrology.Tests()
    denominative.Tests()
    digression.Tests()
    dissonance.Tests()
    distillate.Tests()
    diuretic.Tests()
    embarcadero.Tests()
    enigmatic.Tests()
    escapade.Tests()
    escritoire.Tests()
    ethereal.Tests()
    eucalyptus.Tests()
    exegesis.Tests()
    exoskeleton.Tests()
    frontier.Tests()
    fulminate.Tests()
    galilei.Tests()
    gastronomy.Tests()
    geodesy.Tests()
    gesticulate.Tests()
    gigantism.Tests()
    gnossienne.Tests()
    gossamer.Tests()
    guillotine.Tests()
    hallucination.Tests()
    harlequin.Tests()
    hellenism.Tests()
    hieroglyph.Tests()
    honeycomb.Tests()
    hyperbole.Tests()
    hypotenuse.Tests()
    imperial.Tests()
    inimitable.Tests()
    iridescence.Tests()
    jacinta.Tests()
    kaleidoscope.Tests()
    larceny.Tests()
    legerdemain.Tests()
    locomotion.Tests()
    mandible.Tests()
    mercator.Tests()
    metamorphose.Tests()
    monotonous.Tests()
    mosquito.Tests()
    nomenclature.Tests()
    obligatory.Tests()
    octogenarian.Tests()
    //orthodoxy.Tests()
    panopticon.Tests()
    parasite.Tests()
    perihelion.Tests()
    phoenicia.Tests()
    polaris.Tests()
    plutocrat.Tests()
    polysyllabic.Tests()
    polyvinyl.Tests()
    prepositional.Tests()
    probably.Tests()
    profanity.Tests()
    proscenium.Tests()
    punctuation.Tests()
    quantitative.Tests()
    querencia.Tests()
    reliquary.Tests()
    revolution.Tests()
    rudiments.Tests()
    savagery.Tests()
    scintillate.Tests()
    sedentary.Tests()
    serpentine.Tests()
    spectacular.Tests()
    stenography.Tests()
    stratiform.Tests()
    superlunary.Tests()
    surveillance.Tests()
    synesthesia.Tests()
    symbolism.Tests()
    tarantula.Tests()
    telekinesis.Tests()
    typonym.Tests()
    ultimatum.Tests()
    ulysses.Tests()
    //umbrageous.Tests() - lib/umbrageous test file is an example, not a Tests suite
    urticose.Tests()
    vexillology.Tests()
    vacuous.Tests()
    vicarious.Tests()
    vivisection.Tests()
    jacinta.RecordsTests()
    jacinta.ValidationTests()
    wisteria.Tests()
    xenophile.Tests()
    xylophone.Tests()
    ypsiloid.Tests()
    yossarian.Tests()
    zephyrine.Tests()
    zeppelin.Tests()
    ziggurat.Tests()

object FailingTests extends Suite(m"Failing tests"):
  def run(): Unit =
    // turbulence.Tests() - deadlock
    ()

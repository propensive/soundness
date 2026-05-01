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
┃    Soundness, version 0.54.0.                                                                    ┃
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
    austronesian.Tests()
    aviation.Tests()
    baroque.Tests()
    bitumen.Tests()
    burdock.Tests()
    cacophony.Tests()
    caduceus.Tests()
    caesura.Tests()
    camouflage.Tests()
    capricious.Tests()
    cardinality.Tests()
    cataclysm.Tests()
    cellulose.Tests()
    charisma.Tests()
    chiaroscuro.Tests()
    //coaxial.Tests()
    _root_.contextual.Tests()
    contingency.Tests()
    //cosmopolite.Tests()
    decorum.Tests()
    dendrology.Tests()
    denominative.Tests()
    digression.Tests()
    dissonance.Tests()
    distillate.Tests()
    diuretic.Tests()
    enigmatic.Tests()
    //embarcadero.Tests()
    //escapade.Tests()
    //escritoire.Tests()
    ethereal.Tests()
    //eucalyptus.Tests()
    exegesis.Tests()
    exoskeleton.Tests()
    //frontier.Tests()
    fulminate.Tests()
    galilei.Tests()
    gastronomy.Tests()
    geodesy.Tests()
    gesticulate.Tests()
    //gigantism.Tests()
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
    //legerdemain.Tests()
    mandible.Tests()
    mercator.Tests()
    metamorphose.Tests()
    monotonous.Tests()
    mosquito.Tests()
    nomenclature.Tests()
    //obligatory.Tests() - obligatory_test.scala declares `package synesthesia`
    octogenarian.Tests()
    //orthodoxy.Tests()
    panopticon.Tests()
    parasite.Tests()
    //perihelion.Tests()
    phoenicia.Tests()
    polaris.Tests()
    plutocrat.Tests()
    polyvinyl.Tests()
    prepositional.Tests()
    probably.Tests()
    profanity.Tests()
    proscenium.Tests()
    punctuation.Tests()
    quantitative.Tests()
    revolution.Tests()
    rudiments.Tests()
    savagery.Tests()
    scintillate.Tests()
    sedentary.Tests()
    serpentine.Tests()
    spectacular.Tests()
    stenography.Tests()
    superlunary.Tests()
    surveillance.Tests()
    synesthesia.Tests()
    symbolism.Tests()
    tarantula.Tests()
    typonym.Tests()
    ulysses.Tests()
    //umbrageous.Tests() - lib/umbrageous test file is an example, not a Tests suite
    urticose.Tests()
    vexillology.Tests()
    vacuous.Tests()
    vicarious.Tests()
    villainy.Tests()
    // wisteria.Tests() - temporarily disabled
    xylophone.Tests()
    yossarian.Tests()
    zephyrine.Tests()
    //zeppelin.Tests()
    //ziggurat.Tests() - ziggurat.test missing from build.mill test bundle

object FailingTests extends Suite(m"Failing tests"):
  def run(): Unit =
    merino.Tests()
    satirical.Tests()
    telekinesis.Tests()
    // turbulence.Tests() - deadlock

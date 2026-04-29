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
    aviation.Tests()
    baroque.Tests()
    //bitumen.Tests()
    burdock.Tests()
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
    //exegesis.Tests()
    exoskeleton.Tests()
    feudalism.Tests()
    //frontier.Tests()
    fulminate.Tests()
    galilei.Tests()
    //gastronomy.Tests()
    geodesy.Tests()
    gesticulate.Tests()
    //gigantism.Tests()
    //gnossienne.Tests()
    gossamer.Tests()
    //guillotine.Tests()
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
    //journalist.Tests()
    kaleidoscope.Tests()
    larceny.Tests()
    //legerdemain.Tests()
    mercator.Tests()
    metamorphose.Tests()
    monotonous.Tests()
    mosquito.Tests()
    nomenclature.Tests()
    //obligatory.Tests()
    octogenarian.Tests()
    //orthodoxy.Tests()
    panopticon.Tests()
    //perihelion.Tests()
    phoenicia.Tests()
    polaris.Tests()
    plutocrat.Tests()
    //polyvinyl.Tests()
    prepositional.Tests()
    probably.Tests()
    profanity.Tests()
    proscenium.Tests()
    punctuation.Tests()
    quantitative.Tests()
    revolution.Tests()
    rudiments.Tests()
    //savagery.Tests()
    scintillate.Tests()
    sedentary.Tests()
    serpentine.Tests()
    spectacular.Tests()
    stenography.Tests()
    //superlunary.Tests()
    surveillance.Tests()
    //synesthesia.Tests()
    symbolism.Tests
    tarantula.Tests()
    typonym.Tests()
    ulysses.Tests()
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

object FailingTests extends Suite(m"Failing tests"):
  def run(): Unit =
    //austronesian.Tests() - crashing on compile
    mandible.Tests()
    //merino.Tests() - crashing
    parasite.Tests()
    //satirical.Tests() - crashing
    //telekinesis.Tests() - crashing
    // turbulence.Tests() - deadlock
    //umbrageous.Tests()

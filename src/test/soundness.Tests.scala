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
┃    Soundness, version 0.46.0.                                                                    ┃
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
    contextual.Tests()
    contingency.Tests()
    dendrology.Tests()
    denominative.Tests()
    digression.Tests()
    dissonance.Tests()
    distillate.Tests()
    diuretic.Tests()
    enigmatic.Tests()
    ethereal.Tests()
    exoskeleton.Tests()
    feudalism.Tests()
    fulminate.Tests()
    galilei.Tests()
    geodesy.Tests()
    gesticulate.Tests()
    gossamer.Tests()
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
    mercator.Tests()
    metamorphose.Tests()
    monotonous.Tests()
    mosquito.Tests()
    urticose.Tests()
    nomenclature.Tests()
    octogenarian.Tests()
    panopticon.Tests()
    //perihelion.Tests()
    phoenicia.Tests()
    polaris.Tests()
    prepositional.Tests()
    probably.Tests()
    profanity.Tests()
    proscenium.Tests()
    punctuation.Tests()
    quantitative.Tests()
    revolution.Tests()
    rudiments.Tests()
    scintillate.Tests()
    sedentary.Tests()
    serpentine.Tests()
    spectacular.Tests()
    stenography.Tests()
    surveillance.Tests()
    symbolism.Tests
    tarantula.Tests()
    typonym.Tests()
    ulysses.Tests()
    vexillology.Tests()
    vacuous.Tests()
    vicarious.Tests()
    wisteria.Tests()
    yossarian.Tests()
    zephyrine.Tests()

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

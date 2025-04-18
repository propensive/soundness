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
┃    Soundness, version 0.27.0.                                                                    ┃
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
    burdock.Tests()
    caesura.Tests()
    camouflage.Tests()
    capricious.Tests()
    cardinality.Tests()
    cataclysm.Tests()
    charisma.Tests()
    contextual.Tests()
    contingency.Tests()
    denominative.Tests()
    digression.Tests()
    dissonance.Tests()
    distillate.Tests()
    feudalism.Tests()
    fulminate.Tests()
    galilei.Tests()
    geodesy.Tests()
    gesticulate.Tests()
    gossamer.Tests()
    hellenism.Tests()
    honeycomb.Tests()
    hyperbole.Tests()
    hypotenuse.Tests()
    imperial.Tests()
    inimitable.Tests()
    iridescence.Tests()
    jacinta.Tests()
    kaleidoscope.Tests()
    larceny.Tests()
    metamorphose.Tests()
    nettlesome.Tests()
    nomenclature.Tests()
    octogenarian.Tests()
    panopticon.Tests()
    phoenicia.Tests()
    polaris.Tests()
    prepositional.Tests()
    proscenium.Tests()
    revolution.Tests()
    rudiments.Tests()
    sedentary.Tests()
    serpentine.Tests()
    spectacular.Tests
    symbolism.Tests
    turbulence.Tests()
    typonym.Tests()
    ulysses.Tests()
    vexillology.Tests()
    vacuous.Tests()
    vicarious.Tests()
    wisteria.Tests()
    zephyrine.Tests()

object FailingTests extends Suite(m"Failing tests"):
  def run(): Unit =
    baroque.Tests()
    cellulose.Tests()
    chiaroscuro.Tests()
    mandible.Tests()
    monotonous.Tests()
    mosquito.Tests()
    quantitative.Tests()
    //satirical.Tests() - crashing
    //telekinesis.Tests() - crashing
    //umbrageous.Tests()

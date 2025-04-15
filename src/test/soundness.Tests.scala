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
    adversaria.Tests()
    anamnesis.Tests()
    austronesian.Tests()
    aviation.Tests()
    caesura.Tests()
    camouflage.Tests()
    capricious.Tests()
    cardinality.Tests()
    contingency.Tests()
    digression.Tests()
    dissonance.Tests()
    distillate.Tests()
    geodesy.Tests()
    gesticulate.Tests()
    gossamer.Tests()
    inimitable.Tests()
    jacinta.Tests()
    kaleidoscope.Tests()
    metamorphose.Tests()
    nettlesome.Tests()
    nomenclature.Tests()
    panopticon.Tests()
    rudiments.Tests()
    spectacular.Tests
    turbulence.Tests()
    typonym.Tests()
    vexillology.Tests()
    zephyrine.Tests()

object FailingTests extends Suite(m"Failing tests"):
  def run(): Unit =
   acyclicity.Tests()
   ambience.Tests()
   anthology.Tests()
   anticipation.Tests()
   baroque.Tests()
   //burdock.Tests()
   cataclysm.Tests()
   charisma.Tests()
   chiaroscuro.Tests()
   contextual.Tests()
   //denominative.Tests()
   fulminate.Tests()
   //galilei.Tests()
   //hellenism.Tests()
   //hyperbole.Tests()
   //larceny.Tests()
   mandible.Tests()
   monotonous.Tests()
   mosquito.Tests()
   //octogenarian.Tests()
   //phoenicia.Tests()
   //polaris.Tests()
   //prepositional.Tests()
   //proscenium.Tests()
   quantitative.Tests()
   //revolution.Tests()
   //satirical.Tests() - crashing
   //sedentary.Tests()
   //serpentine.Tests()
   //telekinesis.Tests() - crashing
   //umbrageous.Tests()
   //vacuous.Tests()
   //vicarious.Tests()
   wisteria.Tests()

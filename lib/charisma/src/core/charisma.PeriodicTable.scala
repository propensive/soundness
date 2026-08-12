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
package charisma

import proscenium.compat.*

import anticipation.*
import contingency.*
import gossamer.*
import hypotenuse.*
import denominative.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object PeriodicTable:
  val H = Chemical.Element(1, t"H", t"Hydrogen")
  val He = Chemical.Element(2,   t"He", t"Helium")
  val Li = Chemical.Element(3,   t"Li", t"Lithium")
  val Be = Chemical.Element(4,   t"Be", t"Beryllium")
  val B = Chemical.Element(5,   t"B",  t"Boron")
  val C = Chemical.Element(6,   t"C",  t"Carbon")
  val N = Chemical.Element(7,   t"N",  t"Nitrogen")
  val O = Chemical.Element(8,   t"O",  t"Oxygen")
  val F = Chemical.Element(9,   t"F",  t"Fluorine")
  val Ne = Chemical.Element(10,  t"Ne", t"Neon")
  val Na = Chemical.Element(11,  t"Na", t"Sodium")
  val Mg = Chemical.Element(12,  t"Mg", t"Magnesium")
  val Al = Chemical.Element(13,  t"Al", t"Aluminium")
  val Si = Chemical.Element(14,  t"Si", t"Silicon")
  val P = Chemical.Element(15,  t"P",  t"Phosphorus")
  val S = Chemical.Element(16,  t"S",  t"Sulphur")
  val Cl = Chemical.Element(17,  t"Cl", t"Chlorine")
  val Ar = Chemical.Element(18,  t"Ar", t"Argon")
  val K = Chemical.Element(19, t"K", t"Potassium")
  val Ca = Chemical.Element(20,  t"Ca", t"Calcium")
  val Sc = Chemical.Element(21,  t"Sc", t"Scandium")
  val Ti = Chemical.Element(22,  t"Ti", t"Titanium")
  val V = Chemical.Element(23, t"V", t"Vanadium")
  val Cr = Chemical.Element(24,  t"Cr", t"Chromium")
  val Mn = Chemical.Element(25,  t"Mn", t"Manganese")
  val Fe = Chemical.Element(26,  t"Fe", t"Iron")
  val Co = Chemical.Element(27,  t"Co", t"Cobalt")
  val Ni = Chemical.Element(28,  t"Ni", t"Nickel")
  val Cu = Chemical.Element(29,  t"Cu", t"Copper")
  val Zn = Chemical.Element(30,  t"Zn", t"Zinc")
  val Ga = Chemical.Element(31,  t"Ga", t"Gallium")
  val Ge = Chemical.Element(32,  t"Ge", t"Germanium")
  val As = Chemical.Element(33,  t"As", t"Arsenic")
  val Se = Chemical.Element(34,  t"Se", t"Selenium")
  val Br = Chemical.Element(35,  t"Br", t"Bromine")
  val Kr = Chemical.Element(36,  t"Kr", t"Krypton")
  val Rb = Chemical.Element(37,  t"Rb", t"Rubidium")
  val Sr = Chemical.Element(38,  t"Sr", t"Strontium")
  val Y = Chemical.Element(39, t"Y", t"Yttrium")
  val Zr = Chemical.Element(40,  t"Zr", t"Zirconium")
  val Nb = Chemical.Element(41,  t"Nb", t"Niobium")
  val Mo = Chemical.Element(42,  t"Mo", t"Molybdenum")
  val Tc = Chemical.Element(43,  t"Tc", t"Technetium")
  val Ru = Chemical.Element(44,  t"Ru", t"Ruthenium")
  val Rh = Chemical.Element(45,  t"Rh", t"Rhodium")
  val Pd = Chemical.Element(46,  t"Pd", t"Palladium")
  val Ag = Chemical.Element(47,  t"Ag", t"Silver")
  val Cd = Chemical.Element(48,  t"Cd", t"Cadmium")
  val In = Chemical.Element(49,  t"In", t"Indium")
  val Sn = Chemical.Element(50,  t"Sn", t"Tin")
  val Sb = Chemical.Element(51,  t"Sb", t"Antimony")
  val Te = Chemical.Element(52,  t"Te", t"Tellurium")
  val I = Chemical.Element(53, t"I", t"Iodine")
  val Xe = Chemical.Element(54,  t"Xe", t"Xenon")
  val Cs = Chemical.Element(55,  t"Cs", t"Cesium")
  val Ba = Chemical.Element(56,  t"Ba", t"Barium")
  val La = Chemical.Element(57,  t"La", t"Lanthanum")
  val Ce = Chemical.Element(58,  t"Ce", t"Cerium")
  val Pr = Chemical.Element(59,  t"Pr", t"Praseodymium")
  val Nd = Chemical.Element(60,  t"Nd", t"Neodymium")
  val Pm = Chemical.Element(61,  t"Pm", t"Promethium")
  val Sm = Chemical.Element(62,  t"Sm", t"Samarium")
  val Eu = Chemical.Element(63,  t"Eu", t"Europium")
  val Gd = Chemical.Element(64,  t"Gd", t"Gadolinium")
  val Tb = Chemical.Element(65,  t"Tb", t"Terbium")
  val Dy = Chemical.Element(66,  t"Dy", t"Dysprosium")
  val Ho = Chemical.Element(67,  t"Ho", t"Holmium")
  val Er = Chemical.Element(68,  t"Er", t"Erbium")
  val Tm = Chemical.Element(69,  t"Tm", t"Thulium")
  val Yb = Chemical.Element(70,  t"Yb", t"Ytterbium")
  val Lu = Chemical.Element(71,  t"Lu", t"Lutetium")
  val Hf = Chemical.Element(72,  t"Hf", t"Hafnium")
  val Ta = Chemical.Element(73,  t"Ta", t"Tantalum")
  val W = Chemical.Element(74, t"W", t"Tungsten")
  val Re = Chemical.Element(75,  t"Re", t"Rhenium")
  val Os = Chemical.Element(76,  t"Os", t"Osmium")
  val Ir = Chemical.Element(77,  t"Ir", t"Iridium")
  val Pt = Chemical.Element(78,  t"Pt", t"Platinum")
  val Au = Chemical.Element(79,  t"Au", t"Gold")
  val Hg = Chemical.Element(80,  t"Hg", t"Mercury")
  val Tl = Chemical.Element(81,  t"Tl", t"Thallium")
  val Pb = Chemical.Element(82,  t"Pb", t"Lead")
  val Bi = Chemical.Element(83,  t"Bi", t"Bismuth")
  val Po = Chemical.Element(84,  t"Po", t"Polonium")
  val At = Chemical.Element(85,  t"At", t"Astatine")
  val Rn = Chemical.Element(86,  t"Rn", t"Radon")
  val Fr = Chemical.Element(87,  t"Fr", t"Francium")
  val Ra = Chemical.Element(88,  t"Ra", t"Radium")
  val Ac = Chemical.Element(89,  t"Ac", t"Actinium")
  val Th = Chemical.Element(90,  t"Th", t"Thorium")
  val Pa = Chemical.Element(91,  t"Pa", t"Protactinium")
  val U = Chemical.Element(92, t"U", t"Uranium")
  val Np = Chemical.Element(93,  t"Np", t"Neptunium")
  val Pu = Chemical.Element(94,  t"Pu", t"Plutonium")
  val Am = Chemical.Element(95,  t"Am", t"Americium")
  val Cm = Chemical.Element(96,  t"Cm", t"Curium")
  val Bk = Chemical.Element(97,  t"Bk", t"Berkelium")
  val Cf = Chemical.Element(98,  t"Cf", t"Californium")
  val Es = Chemical.Element(99,  t"Es", t"Einsteinium")
  val Fm = Chemical.Element(100, t"Fm", t"Fermium")
  val Md = Chemical.Element(101, t"Md", t"Mendelevium")
  val No = Chemical.Element(102, t"No", t"Nobelium")
  val Lr = Chemical.Element(103, t"Lr", t"Lawrencium")
  val Rf = Chemical.Element(104, t"Rf", t"Rutherfordium")
  val Db = Chemical.Element(105, t"Db", t"Dubnium")
  val Sg = Chemical.Element(106, t"Sg", t"Seaborgium")
  val Bh = Chemical.Element(107, t"Bh", t"Bohrium")
  val Hs = Chemical.Element(108, t"Hs", t"Hassium")
  val Mt = Chemical.Element(109, t"Mt", t"Meitnerium")
  val Ds = Chemical.Element(110, t"Ds", t"Darmstadtium")
  val Rg = Chemical.Element(111, t"Rg", t"Roentgenium")
  val Cn = Chemical.Element(112, t"Cn", t"Copernicium")
  val Nh = Chemical.Element(113, t"Nh", t"Nihonium")
  val Fl = Chemical.Element(114, t"Fl", t"Flerovium")
  val Mc = Chemical.Element(115, t"Mc", t"Moscovium")
  val Lv = Chemical.Element(116, t"Lv", t"Livermorium")
  val Ts = Chemical.Element(117, t"Ts", t"Tennessine")
  val Og = Chemical.Element(118, t"Og", t"Oganesson")

  val elements: Array[Chemical.Element]^{} =
    Array.of
      ( H, He, Li, Be, B, C, N, O, F, Ne, Na, Mg, Al, Si, P, S, Cl, Ar, K, Ca, Sc, Ti, V, Cr, Mn,
        Fe, Co, Ni, Cu, Zn, Ga, Ge, As, Se, Br, Kr, Rb, Sr, Y, Zr, Nb, Mo, Tc, Ru, Rh, Pd, Ag, Cd,
        In, Sn, Sb, Te, I, Xe, Cs, Ba, La, Ce, Pr, Nd, Pm, Sm, Eu, Gd, Tb, Dy, Ho, Er, Tm, Yb, Lu,
        Hf, Ta, W, Re, Os, Ir, Pt, Au, Hg, Tl, Pb, Bi, Po, At, Rn, Fr, Ra, Ac, Th, Pa, U, Np, Pu,
        Am, Cm, Bk, Cf, Es, Fm, Md, No, Lr, Rf, Db, Sg, Bh, Hs, Mt, Ds, Rg, Cn, Nh, Fl, Mc, Lv, Ts,
        Og )

  lazy val symbols: Map[Text, Chemical.Element] = unsafely(elements.readable.indexBy(_.symbol))

  def apply(number: Int): Optional[Chemical.Element] =
    elements.at(Ordinal.zerary(number - 1))

  def apply(symbol: Text): Optional[Chemical.Element] = symbols(symbol).or(Unset)

  private val prefixes: Array[Text]^{} =
    Array.of(t"nil", t"un", t"bi", t"tri", t"quad", t"pent", t"hex", t"sept", t"oct", t"enn")

  def element(number: Int): Chemical.Element =
    def recur(name: Text, symbol: Text, digits: Int): Chemical.Element =
      if digits == 0
      then Chemical.Element(number, symbol.capitalize, name.capitalize.sub(t"ii", t"i"))
      else
        val prefix = prefixes.readUnchecked(digits%10)
        recur(prefix+name, t"${prefix.s.charAt(0)}$symbol", digits/10)

    recur(t"ium", t"", number)

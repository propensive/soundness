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
  val H = Chemical.Element(1, "H", "Hydrogen")
  val He = Chemical.Element(2,   "He", "Helium")
  val Li = Chemical.Element(3,   "Li", "Lithium")
  val Be = Chemical.Element(4,   "Be", "Beryllium")
  val B = Chemical.Element(5,   "B",  "Boron")
  val C = Chemical.Element(6,   "C",  "Carbon")
  val N = Chemical.Element(7,   "N",  "Nitrogen")
  val O = Chemical.Element(8,   "O",  "Oxygen")
  val F = Chemical.Element(9,   "F",  "Fluorine")
  val Ne = Chemical.Element(10,  "Ne", "Neon")
  val Na = Chemical.Element(11,  "Na", "Sodium")
  val Mg = Chemical.Element(12,  "Mg", "Magnesium")
  val Al = Chemical.Element(13,  "Al", "Aluminium")
  val Si = Chemical.Element(14,  "Si", "Silicon")
  val P = Chemical.Element(15,  "P",  "Phosphorus")
  val S = Chemical.Element(16,  "S",  "Sulphur")
  val Cl = Chemical.Element(17,  "Cl", "Chlorine")
  val Ar = Chemical.Element(18,  "Ar", "Argon")
  val K = Chemical.Element(19, "K", "Potassium")
  val Ca = Chemical.Element(20,  "Ca", "Calcium")
  val Sc = Chemical.Element(21,  "Sc", "Scandium")
  val Ti = Chemical.Element(22,  "Ti", "Titanium")
  val V = Chemical.Element(23, "V", "Vanadium")
  val Cr = Chemical.Element(24,  "Cr", "Chromium")
  val Mn = Chemical.Element(25,  "Mn", "Manganese")
  val Fe = Chemical.Element(26,  "Fe", "Iron")
  val Co = Chemical.Element(27,  "Co", "Cobalt")
  val Ni = Chemical.Element(28,  "Ni", "Nickel")
  val Cu = Chemical.Element(29,  "Cu", "Copper")
  val Zn = Chemical.Element(30,  "Zn", "Zinc")
  val Ga = Chemical.Element(31,  "Ga", "Gallium")
  val Ge = Chemical.Element(32,  "Ge", "Germanium")
  val As = Chemical.Element(33,  "As", "Arsenic")
  val Se = Chemical.Element(34,  "Se", "Selenium")
  val Br = Chemical.Element(35,  "Br", "Bromine")
  val Kr = Chemical.Element(36,  "Kr", "Krypton")
  val Rb = Chemical.Element(37,  "Rb", "Rubidium")
  val Sr = Chemical.Element(38,  "Sr", "Strontium")
  val Y = Chemical.Element(39, "Y", "Yttrium")
  val Zr = Chemical.Element(40,  "Zr", "Zirconium")
  val Nb = Chemical.Element(41,  "Nb", "Niobium")
  val Mo = Chemical.Element(42,  "Mo", "Molybdenum")
  val Tc = Chemical.Element(43,  "Tc", "Technetium")
  val Ru = Chemical.Element(44,  "Ru", "Ruthenium")
  val Rh = Chemical.Element(45,  "Rh", "Rhodium")
  val Pd = Chemical.Element(46,  "Pd", "Palladium")
  val Ag = Chemical.Element(47,  "Ag", "Silver")
  val Cd = Chemical.Element(48,  "Cd", "Cadmium")
  val In = Chemical.Element(49,  "In", "Indium")
  val Sn = Chemical.Element(50,  "Sn", "Tin")
  val Sb = Chemical.Element(51,  "Sb", "Antimony")
  val Te = Chemical.Element(52,  "Te", "Tellurium")
  val I = Chemical.Element(53, "I", "Iodine")
  val Xe = Chemical.Element(54,  "Xe", "Xenon")
  val Cs = Chemical.Element(55,  "Cs", "Cesium")
  val Ba = Chemical.Element(56,  "Ba", "Barium")
  val La = Chemical.Element(57,  "La", "Lanthanum")
  val Ce = Chemical.Element(58,  "Ce", "Cerium")
  val Pr = Chemical.Element(59,  "Pr", "Praseodymium")
  val Nd = Chemical.Element(60,  "Nd", "Neodymium")
  val Pm = Chemical.Element(61,  "Pm", "Promethium")
  val Sm = Chemical.Element(62,  "Sm", "Samarium")
  val Eu = Chemical.Element(63,  "Eu", "Europium")
  val Gd = Chemical.Element(64,  "Gd", "Gadolinium")
  val Tb = Chemical.Element(65,  "Tb", "Terbium")
  val Dy = Chemical.Element(66,  "Dy", "Dysprosium")
  val Ho = Chemical.Element(67,  "Ho", "Holmium")
  val Er = Chemical.Element(68,  "Er", "Erbium")
  val Tm = Chemical.Element(69,  "Tm", "Thulium")
  val Yb = Chemical.Element(70,  "Yb", "Ytterbium")
  val Lu = Chemical.Element(71,  "Lu", "Lutetium")
  val Hf = Chemical.Element(72,  "Hf", "Hafnium")
  val Ta = Chemical.Element(73,  "Ta", "Tantalum")
  val W = Chemical.Element(74, "W", "Tungsten")
  val Re = Chemical.Element(75,  "Re", "Rhenium")
  val Os = Chemical.Element(76,  "Os", "Osmium")
  val Ir = Chemical.Element(77,  "Ir", "Iridium")
  val Pt = Chemical.Element(78,  "Pt", "Platinum")
  val Au = Chemical.Element(79,  "Au", "Gold")
  val Hg = Chemical.Element(80,  "Hg", "Mercury")
  val Tl = Chemical.Element(81,  "Tl", "Thallium")
  val Pb = Chemical.Element(82,  "Pb", "Lead")
  val Bi = Chemical.Element(83,  "Bi", "Bismuth")
  val Po = Chemical.Element(84,  "Po", "Polonium")
  val At = Chemical.Element(85,  "At", "Astatine")
  val Rn = Chemical.Element(86,  "Rn", "Radon")
  val Fr = Chemical.Element(87,  "Fr", "Francium")
  val Ra = Chemical.Element(88,  "Ra", "Radium")
  val Ac = Chemical.Element(89,  "Ac", "Actinium")
  val Th = Chemical.Element(90,  "Th", "Thorium")
  val Pa = Chemical.Element(91,  "Pa", "Protactinium")
  val U = Chemical.Element(92, "U", "Uranium")
  val Np = Chemical.Element(93,  "Np", "Neptunium")
  val Pu = Chemical.Element(94,  "Pu", "Plutonium")
  val Am = Chemical.Element(95,  "Am", "Americium")
  val Cm = Chemical.Element(96,  "Cm", "Curium")
  val Bk = Chemical.Element(97,  "Bk", "Berkelium")
  val Cf = Chemical.Element(98,  "Cf", "Californium")
  val Es = Chemical.Element(99,  "Es", "Einsteinium")
  val Fm = Chemical.Element(100, "Fm", "Fermium")
  val Md = Chemical.Element(101, "Md", "Mendelevium")
  val No = Chemical.Element(102, "No", "Nobelium")
  val Lr = Chemical.Element(103, "Lr", "Lawrencium")
  val Rf = Chemical.Element(104, "Rf", "Rutherfordium")
  val Db = Chemical.Element(105, "Db", "Dubnium")
  val Sg = Chemical.Element(106, "Sg", "Seaborgium")
  val Bh = Chemical.Element(107, "Bh", "Bohrium")
  val Hs = Chemical.Element(108, "Hs", "Hassium")
  val Mt = Chemical.Element(109, "Mt", "Meitnerium")
  val Ds = Chemical.Element(110, "Ds", "Darmstadtium")
  val Rg = Chemical.Element(111, "Rg", "Roentgenium")
  val Cn = Chemical.Element(112, "Cn", "Copernicium")
  val Nh = Chemical.Element(113, "Nh", "Nihonium")
  val Fl = Chemical.Element(114, "Fl", "Flerovium")
  val Mc = Chemical.Element(115, "Mc", "Moscovium")
  val Lv = Chemical.Element(116, "Lv", "Livermorium")
  val Ts = Chemical.Element(117, "Ts", "Tennessine")
  val Og = Chemical.Element(118, "Og", "Oganesson")

  val elements: Array[Chemical.Element]^{} =
    Array
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
    Array("nil", "un", "bi", "tri", "quad", "pent", "hex", "sept", "oct", "enn")

  def element(number: Int): Chemical.Element =
    def recur(name: Text, symbol: Text, digits: Int): Chemical.Element =
      if digits == 0
      then Chemical.Element(number, symbol.capitalize, name.capitalize.sub("ii", "i"))
      else
        val prefix = prefixes.readUnchecked(digits%10)
        recur(prefix+name, t"${prefix.s.charAt(0)}$symbol", digits/10)

    recur("ium", "", number)

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
┃    Soundness, version 0.31.0 for Scala 3.7.                                                      ┃
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
package cosmopolite

/*import rudiments.*

erased trait Localization
erased trait Language extends Localization
erased trait Country extends Localization
erased trait Dialect[language <: Language, country <: Country]

infix type -[language <: Language, country <: Country] = Dialect[language, country]

erased trait Intercession[language <: Language, dialects <: Tuple]

erased given Intercession[en, (en-US, en-GB, en-AU)] = !!
erased given Intercession[fr, (fr-FR, fr-CA, fr-BE, fr-CH)] = !!

package countries:
  type AF = "AF" & Country
  type AX = "AX" & Country
  type AL = "AL" & Country
  type DZ = "DZ" & Country
  type AS = "AS" & Country
  type AD = "AD" & Country
  type AO = "AO" & Country
  type AI = "AI" & Country
  type AQ = "AQ" & Country
  type AG = "AG" & Country
  type AR = "AR" & Country
  type AM = "AM" & Country
  type AW = "AW" & Country
  type AU = "AU" & Country
  type AT = "AT" & Country
  type AZ = "AZ" & Country
  type BH = "BH" & Country
  type BS = "BS" & Country
  type BD = "BD" & Country
  type BB = "BB" & Country
  type BY = "BY" & Country
  type BE = "BE" & Country
  type BZ = "BZ" & Country
  type BJ = "BJ" & Country
  type BM = "BM" & Country
  type BT = "BT" & Country
  type BO = "BO" & Country
  type BQ = "BQ" & Country
  type BA = "BA" & Country
  type BW = "BW" & Country
  type BV = "BV" & Country
  type BR = "BR" & Country
  type IO = "IO" & Country
  type BN = "BN" & Country
  type BG = "BG" & Country
  type BF = "BF" & Country
  type BI = "BI" & Country
  type KH = "KH" & Country
  type CM = "CM" & Country
  type CA = "CA" & Country
  type CV = "CV" & Country
  type KY = "KY" & Country
  type CF = "CF" & Country
  type TD = "TD" & Country
  type CL = "CL" & Country
  type CN = "CN" & Country
  type CX = "CX" & Country
  type CC = "CC" & Country
  type CO = "CO" & Country
  type KM = "KM" & Country
  type CG = "CG" & Country
  type CD = "CD" & Country
  type CK = "CK" & Country
  type CR = "CR" & Country
  type CI = "CI" & Country
  type HR = "HR" & Country
  type CU = "CU" & Country
  type CW = "CW" & Country
  type CY = "CY" & Country
  type CZ = "CZ" & Country
  type DK = "DK" & Country
  type DJ = "DJ" & Country
  type DM = "DM" & Country
  type DO = "DO" & Country
  type EC = "EC" & Country
  type EG = "EG" & Country
  type SV = "SV" & Country
  type GQ = "GQ" & Country
  type ER = "ER" & Country
  type EE = "EE" & Country
  type ET = "ET" & Country
  type FK = "FK" & Country
  type FO = "FO" & Country
  type FJ = "FJ" & Country
  type FI = "FI" & Country
  type FR = "FR" & Country
  type GF = "GF" & Country
  type PF = "PF" & Country
  type TF = "TF" & Country
  type GA = "GA" & Country
  type GM = "GM" & Country
  type GE = "GE" & Country
  type DE = "DE" & Country
  type GH = "GH" & Country
  type GI = "GI" & Country
  type GR = "GR" & Country
  type GL = "GL" & Country
  type GD = "GD" & Country
  type GP = "GP" & Country
  type GU = "GU" & Country
  type GT = "GT" & Country
  type GG = "GG" & Country
  type GN = "GN" & Country
  type GW = "GW" & Country
  type GY = "GY" & Country
  type HT = "HT" & Country
  type HM = "HM" & Country
  type VA = "VA" & Country
  type HN = "HN" & Country
  type HK = "HK" & Country
  type HU = "HU" & Country
  type IS = "IS" & Country
  type IN = "IN" & Country
  type ID = "ID" & Country
  type IR = "IR" & Country
  type IQ = "IQ" & Country
  type IE = "IE" & Country
  type IM = "IM" & Country
  type IL = "IL" & Country
  type IT = "IT" & Country
  type JM = "JM" & Country
  type JP = "JP" & Country
  type JE = "JE" & Country
  type JO = "JO" & Country
  type KZ = "KZ" & Country
  type KE = "KE" & Country
  type KI = "KI" & Country
  type KP = "KP" & Country
  type KR = "KR" & Country
  type KW = "KW" & Country
  type KG = "KG" & Country
  type LA = "LA" & Country
  type LV = "LV" & Country
  type LB = "LB" & Country
  type LS = "LS" & Country
  type LR = "LR" & Country
  type LY = "LY" & Country
  type LI = "LI" & Country
  type LT = "LT" & Country
  type LU = "LU" & Country
  type MO = "MO" & Country
  type MK = "MK" & Country
  type MG = "MG" & Country
  type MW = "MW" & Country
  type MY = "MY" & Country
  type MV = "MV" & Country
  type ML = "ML" & Country
  type MT = "MT" & Country
  type MH = "MH" & Country
  type MQ = "MQ" & Country
  type MR = "MR" & Country
  type MU = "MU" & Country
  type YT = "YT" & Country
  type MX = "MX" & Country
  type FM = "FM" & Country
  type MD = "MD" & Country
  type MC = "MC" & Country
  type MN = "MN" & Country
  type ME = "ME" & Country
  type MS = "MS" & Country
  type MA = "MA" & Country
  type MZ = "MZ" & Country
  type MM = "MM" & Country
  type NA = "NA" & Country
  type NR = "NR" & Country
  type NP = "NP" & Country
  type NL = "NL" & Country
  type NC = "NC" & Country
  type NZ = "NZ" & Country
  type NI = "NI" & Country
  type NE = "NE" & Country
  type NG = "NG" & Country
  type NU = "NU" & Country
  type NF = "NF" & Country
  type MP = "MP" & Country
  type NO = "NO" & Country
  type OM = "OM" & Country
  type PK = "PK" & Country
  type PW = "PW" & Country
  type PS = "PS" & Country
  type PA = "PA" & Country
  type PG = "PG" & Country
  type PY = "PY" & Country
  type PE = "PE" & Country
  type PH = "PH" & Country
  type PN = "PN" & Country
  type PL = "PL" & Country
  type PT = "PT" & Country
  type PR = "PR" & Country
  type QA = "QA" & Country
  type RE = "RE" & Country
  type RO = "RO" & Country
  type RU = "RU" & Country
  type RW = "RW" & Country
  type BL = "BL" & Country
  type SH = "SH" & Country
  type KN = "KN" & Country
  type LC = "LC" & Country
  type MF = "MF" & Country
  type PM = "PM" & Country
  type VC = "VC" & Country
  type WS = "WS" & Country
  type SM = "SM" & Country
  type ST = "ST" & Country
  type SA = "SA" & Country
  type SN = "SN" & Country
  type RS = "RS" & Country
  type SC = "SC" & Country
  type SL = "SL" & Country
  type SG = "SG" & Country
  type SX = "SX" & Country
  type SK = "SK" & Country
  type SI = "SI" & Country
  type SB = "SB" & Country
  type SO = "SO" & Country
  type ZA = "ZA" & Country
  type GS = "GS" & Country
  type SS = "SS" & Country
  type ES = "ES" & Country
  type LK = "LK" & Country
  type SD = "SD" & Country
  type SR = "SR" & Country
  type SJ = "SJ" & Country
  type SZ = "SZ" & Country
  type SE = "SE" & Country
  type CH = "CH" & Country
  type SY = "SY" & Country
  type TW = "TW" & Country
  type TJ = "TJ" & Country
  type TZ = "TZ" & Country
  type TH = "TH" & Country
  type TL = "TL" & Country
  type TG = "TG" & Country
  type TK = "TK" & Country
  type TO = "TO" & Country
  type TT = "TT" & Country
  type TN = "TN" & Country
  type TR = "TR" & Country
  type TM = "TM" & Country
  type TC = "TC" & Country
  type TV = "TV" & Country
  type UG = "UG" & Country
  type UA = "UA" & Country
  type AE = "AE" & Country
  type GB = "GB" & Country
  type US = "US" & Country
  type UM = "UM" & Country
  type UY = "UY" & Country
  type UZ = "UZ" & Country
  type VU = "VU" & Country
  type VE = "VE" & Country
  type VN = "VN" & Country
  type VG = "VG" & Country
  type VI = "VI" & Country
  type WF = "WF" & Country
  type EH = "EH" & Country
  type YE = "YE" & Country
  type ZM = "ZM" & Country
  type ZW = "ZW" & Country

package languages:
   type aa = "aa" & Language
   type ab = "ab" & Language
   type ae = "ae" & Language
   type af = "af" & Language
   type ak = "ak" & Language
   type am = "am" & Language
   type an = "an" & Language
   type ar = "ar" & Language
   type as = "as" & Language
   type av = "av" & Language
   type ay = "ay" & Language
   type az = "az" & Language
   type ba = "ba" & Language
   type be = "be" & Language
   type bg = "bg" & Language
   type bh = "bh" & Language
   type bi = "bi" & Language
   type bm = "bm" & Language
   type bn = "bn" & Language
   type bo = "bo" & Language
   type br = "br" & Language
   type bs = "bs" & Language
   type ca = "ca" & Language
   type ce = "ce" & Language
   type ch = "ch" & Language
   type co = "co" & Language
   type cr = "cr" & Language
   type cs = "cs" & Language
   type cu = "cu" & Language
   type cv = "cv" & Language
   type cy = "cy" & Language
   type da = "da" & Language
   type de = "de" & Language
   type dv = "dv" & Language
   type dz = "dz" & Language
   type ee = "ee" & Language
   type el = "el" & Language
   type en = "en" & Language
   type eo = "eo" & Language
   type es = "es" & Language
   type et = "et" & Language
   type eu = "eu" & Language
   type fa = "fa" & Language
   type ff = "ff" & Language
   type fi = "fi" & Language
   type fj = "fj" & Language
   type fo = "fo" & Language
   type fr = "fr" & Language
   type fy = "fy" & Language
   type ga = "ga" & Language
   type gd = "gd" & Language
   type gl = "gl" & Language
   type gn = "gn" & Language
   type gu = "gu" & Language
   type gv = "gv" & Language
   type ha = "ha" & Language
   type he = "he" & Language
   type hi = "hi" & Language
   type ho = "ho" & Language
   type hr = "hr" & Language
   type ht = "ht" & Language
   type hu = "hu" & Language
   type hy = "hy" & Language
   type hz = "hz" & Language
   type ia = "ia" & Language
   type id = "id" & Language
   type ie = "ie" & Language
   type ig = "ig" & Language
   type ii = "ii" & Language
   type ik = "ik" & Language
   type io = "io" & Language
   type is = "is" & Language
   type it = "it" & Language
   type iu = "iu" & Language
   type ja = "ja" & Language
   type jv = "jv" & Language
   type ka = "ka" & Language
   type kg = "kg" & Language
   type ki = "ki" & Language
   type kj = "kj" & Language
   type kk = "kk" & Language
   type kl = "kl" & Language
   type km = "km" & Language
   type kn = "kn" & Language
   type ko = "ko" & Language
   type kr = "kr" & Language
   type ks = "ks" & Language
   type ku = "ku" & Language
   type kv = "kv" & Language
   type kw = "kw" & Language
   type ky = "ky" & Language
   type la = "la" & Language
   type lb = "lb" & Language
   type lg = "lg" & Language
   type li = "li" & Language
   type ln = "ln" & Language
   type lo = "lo" & Language
   type lt = "lt" & Language
   type lu = "lu" & Language
   type lv = "lv" & Language
   type mg = "mg" & Language
   type mh = "mh" & Language
   type mi = "mi" & Language
   type mk = "mk" & Language
   type ml = "ml" & Language
   type mn = "mn" & Language
   type mr = "mr" & Language
   type ms = "ms" & Language
   type mt = "mt" & Language
   type my = "my" & Language
   type na = "na" & Language
   type nb = "nb" & Language
   type nd = "nd" & Language
   type ne = "ne" & Language
   type ng = "ng" & Language
   type nl = "nl" & Language
   type nn = "nn" & Language
   type no = "no" & Language
   type nr = "nr" & Language
   type nv = "nv" & Language
   type ny = "ny" & Language
   type oc = "oc" & Language
   type oj = "oj" & Language
   type om = "om" & Language
   type or = "or" & Language
   type os = "os" & Language
   type pa = "pa" & Language
   type pi = "pi" & Language
   type pl = "pl" & Language
   type ps = "ps" & Language
   type pt = "pt" & Language
   type qu = "qu" & Language
   type rm = "rm" & Language
   type rn = "rn" & Language
   type ro = "ro" & Language
   type ru = "ru" & Language
   type rw = "rw" & Language
   type sa = "sa" & Language
   type sc = "sc" & Language
   type sd = "sd" & Language
   type se = "se" & Language
   type sg = "sg" & Language
   type si = "si" & Language
   type sk = "sk" & Language
   type sl = "sl" & Language
   type sm = "sm" & Language
   type sn = "sn" & Language
   type so = "so" & Language
   type sq = "sq" & Language
   type sr = "sr" & Language
   type ss = "ss" & Language
   type st = "st" & Language
   type su = "su" & Language
   type sv = "sv" & Language
   type sw = "sw" & Language
   type ta = "ta" & Language
   type te = "te" & Language
   type tg = "tg" & Language
   type th = "th" & Language
   type ti = "ti" & Language
   type tk = "tk" & Language
   type tl = "tl" & Language
   type tn = "tn" & Language
   type to = "to" & Language
   type tr = "tr" & Language
   type ts = "ts" & Language
   type tt = "tt" & Language
   type tw = "tw" & Language
   type ty = "ty" & Language
   type ug = "ug" & Language
   type uk = "uk" & Language
   type ur = "ur" & Language
   type uz = "uz" & Language
   type ve = "ve" & Language
   type vi = "vi" & Language
   type vo = "vo" & Language
   type wa = "wa" & Language
   type wo = "wo" & Language
   type xh = "xh" & Language
   type yi = "yi" & Language
   type yo = "yo" & Language
   type za = "za" & Language
   type zh = "zh" & Language
   type zu = "zu" & Language
*/

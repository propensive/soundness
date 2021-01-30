package cosmopolite

// https://github.com/propensive/cosmopolite

import scala.reflect._
import scala.util.Not
import scala.annotation._
import scala.language.implicitConversions

object LanguageContext:
   def apply[L2 <: String: ValueOf]: LanguageContext[L2] =
      new LanguageContext(summon[ValueOf[L2]])

case class LanguageContext[L <: String: ValueOf](value: ValueOf[L])

object Messages:
   def make[L <: String: ValueOf](msg: String): Messages[L] =
      Messages[L](Map(summon[ValueOf[L]].value -> msg))

case class Messages[L <: String](text: Map[String, String]):
   def &[L2 <: String](messages: Messages[L2])(using Not[(L | L2) =:= L]): Messages[L | L2] =
      Messages(text ++ messages.text)
   
   def apply[L2 <: L: ValueOf]: String =
      text(summon[ValueOf[L2]].value)

   def apply[L2 <: L]()(using ctx: LanguageContext[L2]): String =
      apply(ctx.value)

import languages.common._

extension (ctx: StringContext):
   
   private def content: String = ctx.parts.head
   
   def en(): Messages["en"] = Messages.make["en"](content)
   def ru(): Messages["ru"] = Messages.make["ru"](content)
   def de(): Messages["de"] = Messages.make["de"](content)
   def es(): Messages["es"] = Messages.make["es"](content)
   def fr(): Messages["fr"] = Messages.make["fr"](content)
   def ja(): Messages["ja"] = Messages.make["ja"](content)
   def pt(): Messages["pt"] = Messages.make["pt"](content)
   def zh(): Messages["zh"] = Messages.make["zh"](content)
   def it(): Messages["it"] = Messages.make["it"](content)
   def pl(): Messages["pl"] = Messages.make["pl"](content)

type MyLangs = En | De | Es | Fr

var dynamicLang = "es"

given LanguageContext[? <: MyLangs] = dynamicLang match
   case "de" => LanguageContext[De]
   case "es" => LanguageContext[Es]
   case "fr" => LanguageContext[Fr]
   case _    => LanguageContext[En]

@main
def run(): Unit =
   val x: Messages[MyLangs] = en"This is in English" & de"Das ist Deutsch" & es"Es español" & fr"français"
   println(x())

object languages:
   object common:
      type En = "en"
      type Fr = "fr"
      type De = "de"
      type Ru = "ru"
      type Pt = "pt"
      type It = "it"
      type Zh = "zh"
      type Es = "es"
      type Jp = "jp"
      type Pl = "pl"
   
   // object all:
   //    opaque type Ab <: Language = Language
   //    opaque type Aa <: Language = Language
   //    opaque type Af <: Language = Language
   //    opaque type Ak <: Language = Language
   //    opaque type Sq <: Language = Language
   //    opaque type Am <: Language = Language
   //    opaque type Ar <: Language = Language
   //    opaque type An <: Language = Language
   //    opaque type Hy <: Language = Language
   //    opaque type As <: Language = Language
   //    opaque type Av <: Language = Language
   //    opaque type Ae <: Language = Language
   //    opaque type Ay <: Language = Language
   //    opaque type Az <: Language = Language
   //    opaque type Bm <: Language = Language
   //    opaque type Ba <: Language = Language
   //    opaque type Eu <: Language = Language
   //    opaque type Be <: Language = Language
   //    opaque type Bn <: Language = Language
   //    opaque type Bh <: Language = Language
   //    opaque type Bi <: Language = Language
   //    opaque type Bs <: Language = Language
   //    opaque type Br <: Language = Language
   //    opaque type Bg <: Language = Language
   //    opaque type My <: Language = Language
   //    opaque type Ca <: Language = Language
   //    opaque type Ch <: Language = Language
   //    opaque type Ce <: Language = Language
   //    opaque type Ny <: Language = Language
   //    opaque type Cv <: Language = Language
   //    opaque type Kw <: Language = Language
   //    opaque type Co <: Language = Language
   //    opaque type Cr <: Language = Language
   //    opaque type Hr <: Language = Language
   //    opaque type Cs <: Language = Language
   //    opaque type Da <: Language = Language
   //    opaque type Dv <: Language = Language
   //    opaque type Nl <: Language = Language
   //    opaque type Dz <: Language = Language
   //    opaque type Eo <: Language = Language
   //    opaque type Et <: Language = Language
   //    opaque type Ee <: Language = Language
   //    opaque type Fo <: Language = Language
   //    opaque type Fj <: Language = Language
   //    opaque type Fi <: Language = Language
   //    opaque type Ff <: Language = Language
   //    opaque type Gl <: Language = Language
   //    opaque type Ka <: Language = Language
   //    opaque type El <: Language = Language
   //    opaque type Gn <: Language = Language
   //    opaque type Gu <: Language = Language
   //    opaque type Ht <: Language = Language
   //    opaque type Ha <: Language = Language
   //    opaque type He <: Language = Language
   //    opaque type Hz <: Language = Language
   //    opaque type Hi <: Language = Language
   //    opaque type Ho <: Language = Language
   //    opaque type Hu <: Language = Language
   //    opaque type Ia <: Language = Language
   //    opaque type Id <: Language = Language
   //    opaque type Ie <: Language = Language
   //    opaque type Ga <: Language = Language
   //    opaque type Ig <: Language = Language
   //    opaque type Ik <: Language = Language
   //    opaque type Io <: Language = Language
   //    opaque type Is <: Language = Language
   //    opaque type Iu <: Language = Language
   //    opaque type Jv <: Language = Language
   //    opaque type Kl <: Language = Language
   //    opaque type Kn <: Language = Language
   //    opaque type Kr <: Language = Language
   //    opaque type Ks <: Language = Language
   //    opaque type Kk <: Language = Language
   //    opaque type Km <: Language = Language
   //    opaque type Ki <: Language = Language
   //    opaque type Rw <: Language = Language
   //    opaque type Ky <: Language = Language
   //    opaque type Kv <: Language = Language
   //    opaque type Kg <: Language = Language
   //    opaque type Ko <: Language = Language
   //    opaque type Ku <: Language = Language
   //    opaque type Kj <: Language = Language
   //    opaque type La <: Language = Language
   //    opaque type Lb <: Language = Language
   //    opaque type Lg <: Language = Language
   //    opaque type Li <: Language = Language
   //    opaque type Ln <: Language = Language
   //    opaque type Lo <: Language = Language
   //    opaque type Lt <: Language = Language
   //    opaque type Lu <: Language = Language
   //    opaque type Lv <: Language = Language
   //    opaque type Gv <: Language = Language
   //    opaque type Mk <: Language = Language
   //    opaque type Mg <: Language = Language
   //    opaque type Ms <: Language = Language
   //    opaque type Ml <: Language = Language
   //    opaque type Mt <: Language = Language
   //    opaque type Mi <: Language = Language
   //    opaque type Mr <: Language = Language
   //    opaque type Mh <: Language = Language
   //    opaque type Mn <: Language = Language
   //    opaque type Na <: Language = Language
   //    opaque type Nv <: Language = Language
   //    opaque type Nd <: Language = Language
   //    opaque type Ne <: Language = Language
   //    opaque type Ng <: Language = Language
   //    opaque type Nb <: Language = Language
   //    opaque type Nn <: Language = Language
   //    opaque type No <: Language = Language
   //    opaque type Ii <: Language = Language
   //    opaque type Nr <: Language = Language
   //    opaque type Oc <: Language = Language
   //    opaque type Oj <: Language = Language
   //    opaque type Cu <: Language = Language
   //    opaque type Om <: Language = Language
   //    opaque type Or <: Language = Language
   //    opaque type Os <: Language = Language
   //    opaque type Pa <: Language = Language
   //    opaque type Pi <: Language = Language
   //    opaque type Fa <: Language = Language
   //    opaque type Ps <: Language = Language
   //    opaque type Qu <: Language = Language
   //    opaque type Rm <: Language = Language
   //    opaque type Rn <: Language = Language
   //    opaque type Ro <: Language = Language
   //    opaque type Sa <: Language = Language
   //    opaque type Sc <: Language = Language
   //    opaque type Sd <: Language = Language
   //    opaque type Se <: Language = Language
   //    opaque type Sm <: Language = Language
   //    opaque type Sg <: Language = Language
   //    opaque type Sr <: Language = Language
   //    opaque type Gd <: Language = Language
   //    opaque type Sn <: Language = Language
   //    opaque type Si <: Language = Language
   //    opaque type Sk <: Language = Language
   //    opaque type Sl <: Language = Language
   //    opaque type So <: Language = Language
   //    opaque type St <: Language = Language
   //    opaque type Su <: Language = Language
   //    opaque type Sw <: Language = Language
   //    opaque type Ss <: Language = Language
   //    opaque type Sv <: Language = Language
   //    opaque type Ta <: Language = Language
   //    opaque type Te <: Language = Language
   //    opaque type Tg <: Language = Language
   //    opaque type Th <: Language = Language
   //    opaque type Ti <: Language = Language
   //    opaque type Bo <: Language = Language
   //    opaque type Tk <: Language = Language
   //    opaque type Tl <: Language = Language
   //    opaque type Tn <: Language = Language
   //    opaque type To <: Language = Language
   //    opaque type Tr <: Language = Language
   //    opaque type Ts <: Language = Language
   //    opaque type Tt <: Language = Language
   //    opaque type Tw <: Language = Language
   //    opaque type Ty <: Language = Language
   //    opaque type Ug <: Language = Language
   //    opaque type Uk <: Language = Language
   //    opaque type Ur <: Language = Language
   //    opaque type Uz <: Language = Language
   //    opaque type Ve <: Language = Language
   //    opaque type Vi <: Language = Language
   //    opaque type Vo <: Language = Language
   //    opaque type Wa <: Language = Language
   //    opaque type Cy <: Language = Language
   //    opaque type Wo <: Language = Language
   //    opaque type Fy <: Language = Language
   //    opaque type Xh <: Language = Language
   //    opaque type Yi <: Language = Language
   //    opaque type Yo <: Language = Language
   //    opaque type Za <: Language = Language
   //    opaque type Zu <: Language = Language

package cosmopolite

// https://github.com/propensive/cosmopolite

import scala.reflect._
import scala.util.Not
import scala.annotation._
import scala.language.implicitConversions

class Language[T <: String & Singleton: ValueOf]():
   type Type = T
   def apply(): String = summon[ValueOf[T]].value

object LanguageContext:
   def apply[L2 <: Language[?]: ClassTag]: LanguageContext[L2] =
      new LanguageContext(summon[ClassTag[L2]])

class LanguageContext[L <: Language[?]](val classTag: ClassTag[L])

object Messages:
   def apply[L <: Language[?]: ClassTag](msg: String): Messages[L] = Messages[L](Map(summon[ClassTag[L]] -> msg))

case class Messages[L <: Language[?]](text: Map[ClassTag[_], String]):
   def &[L2 <: Language[?]](messages: Messages[L2])(using Not[(L | L2) =:= L]): Messages[L | L2] =
     Messages(text ++ messages.text)
   
   def apply[L2 <: L: ClassTag]: String =
     text(summon[ClassTag[L2]])

   def apply[L2 <: L]()(using ctx: LanguageContext[L2]): String =
      apply[L2](using ctx.classTag)

import languages.common._

extension (ctx: StringContext):
   
   private def content: String = ctx.parts.head
   
   def en(): Messages[En] = Messages(content)
   def ru(): Messages[Ru] = Messages(content)
   def de(): Messages[De] = Messages(content)
   def es(): Messages[Es] = Messages(content)
   def fr(): Messages[Fr] = Messages(content)
   def ja(): Messages[Jp] = Messages(content)
   def pt(): Messages[Pt] = Messages(content)
   def zh(): Messages[Zh] = Messages(content)
   def it(): Messages[It] = Messages(content)
   def pl(): Messages[Pl] = Messages(content)

given LanguageContext[Fr] = LanguageContext[Fr]

@main
def run(): Unit =
   println(ValueOf["hello"])
   val x = en"This is in English" & fr"C'est en français" & de"Das ist Deutsch"
   //println(x[En])
   println(x()) // FIXME: This does not seem to terminate

object languages:
   object common:
      opaque type En <: Language[?] = Language["en"]
      opaque type Fr <: Language[?] = Language["fr"]
      opaque type De <: Language[?] = Language["de"]
      opaque type Ru <: Language[?] = Language["ru"]
      opaque type Pt <: Language[?] = Language["pt"]
      opaque type It <: Language[?] = Language["it"]
      opaque type Zh <: Language[?] = Language["zh"]
      opaque type Es <: Language[?] = Language["es"]
      opaque type Jp <: Language[?] = Language["jp"]
      opaque type Pl <: Language[?] = Language["pl"]
   
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

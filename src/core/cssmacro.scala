package cataract

import rudiments.*

import scala.quoted.*

import language.dynamics

type Label = String & Singleton

object Css extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Style =
    ${Macro.read('properties)}

case class Selector(value: String) extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Rule =
    ${Macro.rule('this, 'properties)}
    

object Macro:

  private def words(string: String, acc: List[String] = Nil): List[String] =
    string.indexWhere(_.isUpper, 1) match
      case -1 => (string.toLowerCase :: acc).reverse
      case n  => words(string.drop(n), string.take(n).toLowerCase :: acc)

  def dashed(string: String): String = words(string).join("-")

  def rule(selector: Expr[Selector], props: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Rule] =
    '{Rule($selector, ${read(props)})}


  def read(properties: Expr[Seq[(Label, Any)]])(using Quotes): Expr[Style] =
    import quotes.reflect.*

    def recur(exprs: Seq[Expr[(Label, Any)]]): List[Expr[CssProperty]] =
      exprs match
        case '{($key: k & Label, $value: v)} +: tail =>
          val att = key.valueOrError
          val exp: Expr[CssAttribute[k & Label, v]] =
            Expr.summon[CssAttribute[k & Label, v]].getOrElse {
              val typeName = TypeRepr.of[v].show
              report.error(s"cataract: no valid CSS element $att exists")
              ???
            }
          
          '{CssProperty(dashed($key), $exp.convert($value))} :: recur(tail)
        case _ =>
          Nil
    
    properties match
      case Varargs(exprs) => '{Style(${Expr.ofSeq(recur(exprs))}*)}

case class CssAttribute[Name <: Label, T: ShowProperty](name: Name):
  def convert(value: T): String = summon[ShowProperty[T]].convert(value)

extension (sc: StringContext)
  def css(): Selector = Selector(sc.parts(0))

object CssAttribute:
  given alignContent: CssAttribute["alignContent", String] = CssAttribute("alignContent")
  given alignItems: CssAttribute["alignItems", String] = CssAttribute("alignItems")
  given alignSelf: CssAttribute["alignSelf", String] = CssAttribute("alignSelf")
  given all: CssAttribute["all", String] = CssAttribute("all")
  given animation	: CssAttribute["animation", String] = CssAttribute("animation")
  given animationDelay: CssAttribute["animationDelay", String] = CssAttribute("animationDelay")
  given animationDirection: CssAttribute["animationDirection", String] = CssAttribute("animationDirection")
  given animationDuration: CssAttribute["animationDuration", String] = CssAttribute("animationDuration")
  given animationFillMode: CssAttribute["animationFillMode", String] = CssAttribute("animationFillMode")
  given animationIterationCount: CssAttribute["animationIterationCount", String] = CssAttribute("animationIterationCount")
  given animationName: CssAttribute["animationName", String] = CssAttribute("animationName")
  given animationPlayState: CssAttribute["animationPlayState", String] = CssAttribute("animationPlayState")
  given animationTimingFunction: CssAttribute["animationTimingFunction", String] = CssAttribute("animationTimingFunction")
  given backfaceVisibility: CssAttribute["backfaceVisibility", String] = CssAttribute("backfaceVisibility")
  given background	: CssAttribute["background", String] = CssAttribute("background")
  given backgroundAttachment: CssAttribute["backgroundAttachment", String] = CssAttribute("backgroundAttachment")
  given backgroundBlendMode: CssAttribute["backgroundBlendMode", String] = CssAttribute("backgroundBlendMode")
  given backgroundClip: CssAttribute["backgroundClip", String] = CssAttribute("backgroundClip")
  given backgroundColor: CssAttribute["backgroundColor", String] = CssAttribute("backgroundColor")
  given backgroundImage: CssAttribute["backgroundImage", String] = CssAttribute("backgroundImage")
  given backgroundOrigin: CssAttribute["backgroundOrigin", String] = CssAttribute("backgroundOrigin")
  given backgroundPosition: CssAttribute["backgroundPosition", String] = CssAttribute("backgroundPosition")
  given backgroundRepeat: CssAttribute["backgroundRepeat", String] = CssAttribute("backgroundRepeat")
  given backgroundSize: CssAttribute["backgroundSize", String] = CssAttribute("backgroundSize")
  given border	: CssAttribute["border", String] = CssAttribute("border")
  given borderBottom	: CssAttribute["borderBottom", String] = CssAttribute("borderBottom")
  given borderBottomColor: CssAttribute["borderBottomColor", String] = CssAttribute("borderBottomColor")
  given borderBottomLeftRadius: CssAttribute["borderBottomLeftRadius", String] = CssAttribute("borderBottomLeftRadius")
  given borderBottomRightRadius: CssAttribute["borderBottomRightRadius", String] = CssAttribute("borderBottomRightRadius")
  given borderBottomStyle: CssAttribute["borderBottomStyle", String] = CssAttribute("borderBottomStyle")
  given borderBottomWidth: CssAttribute["borderBottomWidth", String] = CssAttribute("borderBottomWidth")
  given borderCollapse: CssAttribute["borderCollapse", String] = CssAttribute("borderCollapse")
  given borderColor: CssAttribute["borderColor", String] = CssAttribute("borderColor")
  given borderImage	: CssAttribute["borderImage", String] = CssAttribute("borderImage")
  given borderImageOutset: CssAttribute["borderImageOutset", String] = CssAttribute("borderImageOutset")
  given borderImageRepeat: CssAttribute["borderImageRepeat", String] = CssAttribute("borderImageRepeat")
  given borderImageSlice: CssAttribute["borderImageSlice", String] = CssAttribute("borderImageSlice")
  given borderImageSource: CssAttribute["borderImageSource", String] = CssAttribute("borderImageSource")
  given borderImageWidth: CssAttribute["borderImageWidth", String] = CssAttribute("borderImageWidth")
  given borderLeft	: CssAttribute["borderLeft", String] = CssAttribute("borderLeft")
  given borderLeftColor: CssAttribute["borderLeftColor", String] = CssAttribute("borderLeftColor")
  given borderLeftStyle: CssAttribute["borderLeftStyle", String] = CssAttribute("borderLeftStyle")
  given borderLeftWidth: CssAttribute["borderLeftWidth", String] = CssAttribute("borderLeftWidth")
  given borderRadius	: CssAttribute["borderRadius", String] = CssAttribute("borderRadius")
  given borderRight	: CssAttribute["borderRight", String] = CssAttribute("borderRight")
  given borderRightColor: CssAttribute["borderRightColor", String] = CssAttribute("borderRightColor")
  given borderRightStyle: CssAttribute["borderRightStyle", String] = CssAttribute("borderRightStyle")
  given borderRightWidth: CssAttribute["borderRightWidth", String] = CssAttribute("borderRightWidth")
  given borderSpacing: CssAttribute["borderSpacing", String] = CssAttribute("borderSpacing")
  given borderStyle: CssAttribute["borderStyle", String] = CssAttribute("borderStyle")
  given borderTop	: CssAttribute["borderTop", String] = CssAttribute("borderTop")
  given borderTopColor: CssAttribute["borderTopColor", String] = CssAttribute("borderTopColor")
  given borderTopLeftRadius: CssAttribute["borderTopLeftRadius", String] = CssAttribute("borderTopLeftRadius")
  given borderTopRightRadius: CssAttribute["borderTopRightRadius", String] = CssAttribute("borderTopRightRadius")
  given borderTopStyle: CssAttribute["borderTopStyle", String] = CssAttribute("borderTopStyle")
  given borderTopWidth: CssAttribute["borderTopWidth", String] = CssAttribute("borderTopWidth")
  given borderWidth: CssAttribute["borderWidth", String] = CssAttribute("borderWidth")
  given bottom: CssAttribute["bottom", String] = CssAttribute("bottom")
  given boxDecorationBreak: CssAttribute["boxDecorationBreak", String] = CssAttribute("boxDecorationBreak")
  given boxShadow: CssAttribute["boxShadow", String] = CssAttribute("boxShadow")
  given boxSizing: CssAttribute["boxSizing", String] = CssAttribute("boxSizing")
  given breakAfter: CssAttribute["breakAfter", String] = CssAttribute("breakAfter")
  given breakBefore: CssAttribute["breakBefore", String] = CssAttribute("breakBefore")
  given breakInside: CssAttribute["breakInside", String] = CssAttribute("breakInside")
  given captionSide: CssAttribute["captionSide", String] = CssAttribute("captionSide")
  given caretColor: CssAttribute["caretColor", String] = CssAttribute("caretColor")
  given clear: CssAttribute["clear", String] = CssAttribute("clear")
  given clip: CssAttribute["clip", String] = CssAttribute("clip")
  given color: CssAttribute["color", String] = CssAttribute("color")
  given columnCount: CssAttribute["columnCount", String] = CssAttribute("columnCount")
  given columnFill: CssAttribute["columnFill", String] = CssAttribute("columnFill")
  given columnGap: CssAttribute["columnGap", String] = CssAttribute("columnGap")
  given columnRule	: CssAttribute["columnRule", String] = CssAttribute("columnRule")
  given columnRuleColor: CssAttribute["columnRuleColor", String] = CssAttribute("columnRuleColor")
  given columnRuleStyle: CssAttribute["columnRuleStyle", String] = CssAttribute("columnRuleStyle")
  given columnRuleWidth: CssAttribute["columnRuleWidth", String] = CssAttribute("columnRuleWidth")
  given columnSpan: CssAttribute["columnSpan", String] = CssAttribute("columnSpan")
  given columnWidth: CssAttribute["columnWidth", String] = CssAttribute("columnWidth")
  given columns	: CssAttribute["columns", String] = CssAttribute("columns")
  given content: CssAttribute["content", String] = CssAttribute("content")
  given counterIncrement: CssAttribute["counterIncrement", String] = CssAttribute("counterIncrement")
  given counterReset: CssAttribute["counterReset", String] = CssAttribute("counterReset")
  given cursor: CssAttribute["cursor", String] = CssAttribute("cursor")
  given direction: CssAttribute["direction", String] = CssAttribute("direction")
  given display: CssAttribute["display", String] = CssAttribute("display")
  given emptyCells: CssAttribute["emptyCells", String] = CssAttribute("emptyCells")
  given filter: CssAttribute["filter", String] = CssAttribute("filter")
  given flex	: CssAttribute["flex", String] = CssAttribute("flex")
  given flexBasis: CssAttribute["flexBasis", String] = CssAttribute("flexBasis")
  given flexDirection: CssAttribute["flexDirection", String] = CssAttribute("flexDirection")
  given flexFlow	: CssAttribute["flexFlow", String] = CssAttribute("flexFlow")
  given flexGrow: CssAttribute["flexGrow", String] = CssAttribute("flexGrow")
  given flexShrink: CssAttribute["flexShrink", String] = CssAttribute("flexShrink")
  given flexWrap: CssAttribute["flexWrap", String] = CssAttribute("flexWrap")
  given float: CssAttribute["float", String] = CssAttribute("float")
  given font	: CssAttribute["font", String] = CssAttribute("font")
  given fontFamily: CssAttribute["fontFamily", String] = CssAttribute("fontFamily")
  given fontFeatureSettings: CssAttribute["fontFeatureSettings", String] = CssAttribute("fontFeatureSettings")
  given fontKerning: CssAttribute["fontKerning", String] = CssAttribute("fontKerning")
  given fontLanguageOverride: CssAttribute["fontLanguageOverride", String] = CssAttribute("fontLanguageOverride")
  given fontSize: CssAttribute["fontSize", String] = CssAttribute("fontSize")
  given fontSizeAdjust: CssAttribute["fontSizeAdjust", String] = CssAttribute("fontSizeAdjust")
  given fontStretch: CssAttribute["fontStretch", String] = CssAttribute("fontStretch")
  given fontStyle: CssAttribute["fontStyle", String] = CssAttribute("fontStyle")
  given fontSynthesis: CssAttribute["fontSynthesis", String] = CssAttribute("fontSynthesis")
  given fontVariant: CssAttribute["fontVariant", String] = CssAttribute("fontVariant")
  given fontVariantAlternates: CssAttribute["fontVariantAlternates", String] = CssAttribute("fontVariantAlternates")
  given fontVariantCaps: CssAttribute["fontVariantCaps", String] = CssAttribute("fontVariantCaps")
  given fontVariantEastAsian: CssAttribute["fontVariantEastAsian", String] = CssAttribute("fontVariantEastAsian")
  given fontVariantLigatures: CssAttribute["fontVariantLigatures", String] = CssAttribute("fontVariantLigatures")
  given fontVariantNumeric: CssAttribute["fontVariantNumeric", String] = CssAttribute("fontVariantNumeric")
  given fontVariantPosition: CssAttribute["fontVariantPosition", String] = CssAttribute("fontVariantPosition")
  given fontWeight: CssAttribute["fontWeight", String] = CssAttribute("fontWeight")
  given gap	: CssAttribute["gap", String] = CssAttribute("gap")
  given grid	: CssAttribute["grid", String] = CssAttribute("grid")
  given gridArea: CssAttribute["gridArea", String] = CssAttribute("gridArea")
  given gridAutoColumns: CssAttribute["gridAutoColumns", String] = CssAttribute("gridAutoColumns")
  given gridAutoFlow: CssAttribute["gridAutoFlow", String] = CssAttribute("gridAutoFlow")
  given gridAutoRows: CssAttribute["gridAutoRows", String] = CssAttribute("gridAutoRows")
  given gridColumn	: CssAttribute["gridColumn", String] = CssAttribute("gridColumn")
  given gridColumnEnd: CssAttribute["gridColumnEnd", String] = CssAttribute("gridColumnEnd")
  given gridColumnGap: CssAttribute["gridColumnGap", String] = CssAttribute("gridColumnGap")
  given gridColumnStart: CssAttribute["gridColumnStart", String] = CssAttribute("gridColumnStart")
  given gridGap	: CssAttribute["gridGap", String] = CssAttribute("gridGap")
  given gridRow	: CssAttribute["gridRow", String] = CssAttribute("gridRow")
  given gridRowEnd: CssAttribute["gridRowEnd", String] = CssAttribute("gridRowEnd")
  given gridRowGap: CssAttribute["gridRowGap", String] = CssAttribute("gridRowGap")
  given gridRowStart: CssAttribute["gridRowStart", String] = CssAttribute("gridRowStart")
  given gridTemplate	: CssAttribute["gridTemplate", String] = CssAttribute("gridTemplate")
  given gridTemplateAreas: CssAttribute["gridTemplateAreas", String] = CssAttribute("gridTemplateAreas")
  given gridTemplateColumns: CssAttribute["gridTemplateColumns", String] = CssAttribute("gridTemplateColumns")
  given gridTemplateRows: CssAttribute["gridTemplateRows", String] = CssAttribute("gridTemplateRows")
  given hangingPunctuation: CssAttribute["hangingPunctuation", String] = CssAttribute("hangingPunctuation")
  given height: CssAttribute["height", String] = CssAttribute("height")
  given hyphens: CssAttribute["hyphens", String] = CssAttribute("hyphens")
  given imageRendering: CssAttribute["imageRendering", String] = CssAttribute("imageRendering")
  given isolation: CssAttribute["isolation", String] = CssAttribute("isolation")
  given justifyContent: CssAttribute["justifyContent", String] = CssAttribute("justifyContent")
  given left: CssAttribute["left", String] = CssAttribute("left")
  given letterSpacing: CssAttribute["letterSpacing", String] = CssAttribute("letterSpacing")
  given lineBreak: CssAttribute["lineBreak", String] = CssAttribute("lineBreak")
  given lineHeight: CssAttribute["lineHeight", String] = CssAttribute("lineHeight")
  given listStyle: CssAttribute["listStyle", String] = CssAttribute("listStyle")
  given listStyleImage: CssAttribute["listStyleImage", String] = CssAttribute("listStyleImage")
  given listStylePosition: CssAttribute["listStylePosition", String] = CssAttribute("listStylePosition")
  given listStyleType: CssAttribute["listStyleType", String] = CssAttribute("listStyleType")
  given margin: CssAttribute["margin", String] = CssAttribute("margin")
  given marginBottom: CssAttribute["marginBottom", String] = CssAttribute("marginBottom")
  given marginLeft: CssAttribute["marginLeft", String] = CssAttribute("marginLeft")
  given marginRight: CssAttribute["marginRight", String] = CssAttribute("marginRight")
  given marginTop: CssAttribute["marginTop", String] = CssAttribute("marginTop")
  given mask: CssAttribute["mask", String] = CssAttribute("mask")
  given maskType: CssAttribute["maskType", String] = CssAttribute("maskType")
  given maxHeight: CssAttribute["maxHeight", String] = CssAttribute("maxHeight")
  given maxWidth: CssAttribute["maxWidth", String] = CssAttribute("maxWidth")
  given minHeight: CssAttribute["minHeight", String] = CssAttribute("minHeight")
  given minWidth: CssAttribute["minWidth", String] = CssAttribute("minWidth")
  given mixBlendMode: CssAttribute["mixBlendMode", String] = CssAttribute("mixBlendMode")
  given objectFit: CssAttribute["objectFit", String] = CssAttribute("objectFit")
  given objectPosition: CssAttribute["objectPosition", String] = CssAttribute("objectPosition")
  given opacity: CssAttribute["opacity", String] = CssAttribute("opacity")
  given order: CssAttribute["order", String] = CssAttribute("order")
  given orphans: CssAttribute["orphans", String] = CssAttribute("orphans")
  given outline	: CssAttribute["outline", String] = CssAttribute("outline")
  given outlineColor: CssAttribute["outlineColor", String] = CssAttribute("outlineColor")
  given outlineOffset: CssAttribute["outlineOffset", String] = CssAttribute("outlineOffset")
  given outlineStyle: CssAttribute["outlineStyle", String] = CssAttribute("outlineStyle")
  given outlineWidth: CssAttribute["outlineWidth", String] = CssAttribute("outlineWidth")
  given over: CssAttribute["over", String] = CssAttribute("over")
  given overflowWrap: CssAttribute["overflowWrap", String] = CssAttribute("overflowWrap")
  given overflowX: CssAttribute["overflowX", String] = CssAttribute("overflowX")
  given overflowY: CssAttribute["overflowY", String] = CssAttribute("overflowY")
  given padding	: CssAttribute["padding", String] = CssAttribute("padding")
  given paddingBottom: CssAttribute["paddingBottom", String] = CssAttribute("paddingBottom")
  given paddingLeft: CssAttribute["paddingLeft", String] = CssAttribute("paddingLeft")
  given paddingRight: CssAttribute["paddingRight", String] = CssAttribute("paddingRight")
  given paddingTop: CssAttribute["paddingTop", String] = CssAttribute("paddingTop")
  given pageBreakAfter: CssAttribute["pageBreakAfter", String] = CssAttribute("pageBreakAfter")
  given pageBreakBefore: CssAttribute["pageBreakBefore", String] = CssAttribute("pageBreakBefore")
  given pageBreakInside: CssAttribute["pageBreakInside", String] = CssAttribute("pageBreakInside")
  given perspective: CssAttribute["perspective", String] = CssAttribute("perspective")
  given perspectiveOrigin: CssAttribute["perspectiveOrigin", String] = CssAttribute("perspectiveOrigin")
  given pointerEvents: CssAttribute["pointerEvents", String] = CssAttribute("pointerEvents")
  given position: CssAttribute["position", String] = CssAttribute("position")
  given quotes: CssAttribute["quotes", String] = CssAttribute("quotes")
  given resize: CssAttribute["resize", String] = CssAttribute("resize")
  given right: CssAttribute["right", String] = CssAttribute("right")
  given rowGap: CssAttribute["rowGap", String] = CssAttribute("rowGap")
  given scrollBehavior: CssAttribute["scrollBehavior", String] = CssAttribute("scrollBehavior")
  given tabSize: CssAttribute["tabSize", String] = CssAttribute("tabSize")
  given tableLayout: CssAttribute["tableLayout", String] = CssAttribute("tableLayout")
  given textAlign: CssAttribute["textAlign", String] = CssAttribute("textAlign")
  given textAlignLast: CssAttribute["textAlignLast", String] = CssAttribute("textAlignLast")
  given textCombineUpright: CssAttribute["textCombineUpright", String] = CssAttribute("textCombineUpright")
  given textDecoration: CssAttribute["textDecoration", String] = CssAttribute("textDecoration")
  given textDecorationColor: CssAttribute["textDecorationColor", String] = CssAttribute("textDecorationColor")
  given textDecorationLine: CssAttribute["textDecorationLine", String] = CssAttribute("textDecorationLine")
  given textDecorationStyle: CssAttribute["textDecorationStyle", String] = CssAttribute("textDecorationStyle")
  given textIndent: CssAttribute["textIndent", String] = CssAttribute("textIndent")
  given textJustify: CssAttribute["textJustify", String] = CssAttribute("textJustify")
  given textOrientation: CssAttribute["textOrientation", String] = CssAttribute("textOrientation")
  given textOverflow: CssAttribute["textOverflow", String] = CssAttribute("textOverflow")
  given textShadow: CssAttribute["textShadow", String] = CssAttribute("textShadow")
  given textTransform: CssAttribute["textTransform", String] = CssAttribute("textTransform")
  given textUnderlinePosition: CssAttribute["textUnderlinePosition", String] = CssAttribute("textUnderlinePosition")
  given top: CssAttribute["top", String] = CssAttribute("top")
  given transform: CssAttribute["transform", String] = CssAttribute("transform")
  given transformOrigin: CssAttribute["transformOrigin", String] = CssAttribute("transformOrigin")
  given transformStyle: CssAttribute["transformStyle", String] = CssAttribute("transformStyle")
  given transition	: CssAttribute["transition", String] = CssAttribute("transition")
  given transitionDelay: CssAttribute["transitionDelay", String] = CssAttribute("transitionDelay")
  given transitionDuration: CssAttribute["transitionDuration", String] = CssAttribute("transitionDuration")
  given transitionProperty: CssAttribute["transitionProperty", String] = CssAttribute("transitionProperty")
  given transitionTimingFunction: CssAttribute["transitionTimingFunction", String] = CssAttribute("transitionTimingFunction")
  given unicodeBidi: CssAttribute["unicodeBidi", String] = CssAttribute("unicodeBidi")
  given userSelect: CssAttribute["userSelect", String] = CssAttribute("userSelect")
  given verticalAlign: CssAttribute["verticalAlign", String] = CssAttribute("verticalAlign")
  given visibility: CssAttribute["visibility", String] = CssAttribute("visibility")
  given whiteSpace: CssAttribute["whiteSpace", String] = CssAttribute("whiteSpace")
  given widows: CssAttribute["widows", String] = CssAttribute("widows")
  given width: CssAttribute["width", String] = CssAttribute("width")
  given wordBreak: CssAttribute["wordBreak", String] = CssAttribute("wordBreak")
  given wordSpacing: CssAttribute["wordSpacing", String] = CssAttribute("wordSpacing")
  given wordWrap: CssAttribute["wordWrap", String] = CssAttribute("wordWrap")
  given writingMode: CssAttribute["writingMode", String] = CssAttribute("writingMode")
  given zIndex: CssAttribute["zIndex", String] = CssAttribute("zIndex")









object ShowProperty:
  given ShowProperty[Dimension] = _.toString
  
  given ShowProperty[(Dimension, Dimension, Dimension, Dimension)] =
    _.toList.map(_.toString).join(" ")

  given ShowProperty[String] = str => str""""$str""""
  given ShowProperty[Int] = _.toString

trait ShowProperty[T]:
  def convert(value: T): String
  

enum Dimension:
  case Px(value: Double)
  case Pt(value: Double)
  case In(value: Double)
  case Auto
  case Percent(value: Double)
  case Initial
  case Inherit
  case Cm(value: Double)
  case Mm(value: Double)
  case Em(value: Double)
  case Ex(value: Double)
  case Ch(value: Double)
  case Rem(value: Double)
  case Vw(value: Double)
  case Vh(value: Double)
  case Vmin(value: Double)
  case Vmax(value: Double)

  override def toString(): String = this match
    case Px(value)      => str"${value.toString}px"
    case Pt(value)      => str"${value.toString}pt"
    case In(value)      => str"${value.toString}in"
    case Auto           => str"auto"
    case Percent(value) => str"${value.toString}%"
    case Initial        => str"initial"
    case Inherit        => str"inherit"
    case Cm(value)      => str"${value.toString}cm"
    case Mm(value)      => str"${value.toString}mm"
    case Em(value)      => str"${value.toString}em"
    case Ex(value)      => str"${value.toString}ex"
    case Ch(value)      => str"${value.toString}ch"
    case Rem(value)     => str"${value.toString}rem"
    case Vw(value)      => str"${value.toString}vw"
    case Vh(value)      => str"${value.toString}vh"
    case Vmin(value)    => str"${value.toString}vmin"
    case Vmax(value)    => str"${value.toString}vmax"

extension (value: Double)
  def px = Dimension.Px(value)
  def pt = Dimension.Pt(value)
  def in = Dimension.In(value)
  def percent = Dimension.Percent(value)
  def cm = Dimension.Cm(value)
  def mm = Dimension.Mm(value)
  def em = Dimension.Em(value)
  def ex = Dimension.Ex(value)
  def ch = Dimension.Ch(value)
  def rem = Dimension.Rem(value)
  def vw = Dimension.Vw(value)
  def vh = Dimension.Vh(value)
  def vmin = Dimension.Vmin(value)
  def vmax = Dimension.Vmax(value)

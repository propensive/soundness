package rudiments

import annotation.*

transparent inline def contextMessage
    (inline module: Maybe[String] = Unset,
     typeclass: Maybe[String] = Unset,
     param: Maybe[String] = Unset,
     reason: Maybe[String] = Unset,
     suggest: Maybe[String] = Unset)
    (options: ((String, String, String) | (String, String) | String)*)
    : String =

  def italic: String = "\u001b[3m"
  def code: String = "\u001b[2m"
  def bold: String = "\u001b[1m"
  def reset: String = "\u001b[0m"
  
  def prefix: String = inline module match
    case name: String => bold+name+reset+": "
    case _            => ""
  
  def typeclassName = inline typeclass match
    case name: String => code+name+reset
    case _            => "typeclass"
  
  def paramName: String = inline param match
    case name: String => " for the "+code+param+reset+" type parameter"
    case _            => ""
  
  def suggestion: String = inline suggest match
    case name: String => "\n\nFor many purposes, importing "+code+name+reset+" provides a good default."
    case _            => ""

  def padWidth: Int =
    options.map:
      case (ctx, _)    => ctx.length
      case ctx: String => ctx.length
      case (ctx, _, _) => ctx.length
    .max

  def formatOptions: String = options.map:
    case (context, help, required) =>
      val requirements = "(also needs a contextual "+required.map(code+_+reset).mkString(", ")
      
      "  import "+context+(" "*(padWidth - context.length))+"—— "+italic+help+reset+"\n"+
          italic+requirements+reset
    
    case (context, help) =>
      code+"  import "+context+(" "*(padWidth - context.length))+"—— "+italic+help+reset
    
    case context: String =>
      code+"  import "+context+reset
  .mkString("\n")

  val optionsText = inline options.length match
    case 0 => ""
    case 1 => "\n\nThis import may fix the problem:\n\n"+formatOptions
    case _ => "\n\nOne of the following imports may fix the problem:\n\n"+formatOptions
  
  prefix+"a contextual "+typeclassName+" instance is required"+paramName+suggestion+optionsText
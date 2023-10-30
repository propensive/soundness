package exoskeleton

import rudiments.*
import gossamer.*
import spectacular.*
import anticipation.*

trait ArgumentsParser[ParametersType]:
  def apply(textArguments: Iterable[Text])(using CompletionContext): ParametersType

object Parameters:
  def apply
      [ParametersType]
      (textArguments: Iterable[Text])(using parser: ArgumentsParser[ParametersType], context: CompletionContext)
      : ParametersType =
    
    parser(textArguments)

case class Argument(position: Int, value: Text, cursor: Maybe[Int]):
  def apply(): Text = value

  def suggest(fn: => List[Suggestion])(using context: CompletionContext): Unit = context.suggest(position, fn)
  def map(fn: Suggestion => Suggestion)(using context: CompletionContext): Unit = context.map(position, fn)
  
  def restrict(predicate: Suggestion => Boolean)(using context: CompletionContext): Unit =
    context.restrict(position, predicate)

case class PosixParameters
    (positional: List[Argument] = Nil, parameters: Map[Argument, List[Argument]] = Map(),
        postpositional: List[Argument] = Nil)

object ParamDecoder:
  given [ValueType: Decoder]: ParamDecoder[ValueType] =
    case head :: Nil => head().decodeAs[ValueType]
    case _           => Unset

trait ParamDecoder[ValueType]:
  def decode(arguments: List[Argument]): Maybe[ValueType]

class Suggestion
    (text: Text, description: Maybe[Text] = Unset, hidden: Boolean = false, incomplete: Boolean = false)

case class Param[ValueType: ParamDecoder](aliases: List[Char | Text], description: Text)

object SimpleParameterParser extends ArgumentsParser[List[Argument]]:
  def apply(textArguments: Iterable[Text])(using context: CompletionContext): List[Argument] = context.arguments

object PosixArgumentsParser extends ArgumentsParser[PosixParameters]:
  def apply(textArguments: Iterable[Text])(using context: CompletionContext): PosixParameters =

    def recur
        (todo: List[Argument], arguments: List[Argument], current: Maybe[Argument],
            posixParameters: PosixParameters)
        : PosixParameters =
      
      def push(): PosixParameters = current match
        case Unset =>
          PosixParameters(arguments.reverse)
        
        case current: Argument =>
          posixParameters.copy(parameters = posixParameters.parameters.updated(current, arguments.reverse))
      
      todo match
        case head :: tail =>
          if head() == t"--" then push().copy(postpositional = tail)
          else if head().starts(t"-") then recur(tail, Nil, head, push())
          else recur(tail, head :: arguments, current, posixParameters)
        
        case Nil =>
          push()
    
    recur(SimpleParameterParser(textArguments).to(List), Nil, Unset, PosixParameters())

package parameterInterpretation:
  given simple: SimpleParameterParser.type = SimpleParameterParser
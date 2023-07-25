package dissonance

import rudiments.*
import anticipation.*

case class CasualDiffError(issue: CasualDiffError.Issue, line: Int)
extends Error(msg"the diff could not be read because $issue at line $line")

object CasualDiffError:
  enum Issue:
    case InsertionWithoutDeletion
    case BadLineStart(content: Text)
    case DoesNotMatch(content: Text)
  
  given AsMessage[Issue] =
    case Issue.InsertionWithoutDeletion =>
      msg"an insertion was made without a corresponding deletion"
    
    case Issue.BadLineStart(content) =>
      msg"the line $content did not begin with either ${"+".tt} or ${"-".tt} and a space"
    
    case Issue.DoesNotMatch(content) =>
      msg"the line $content could not be found in the document"

case class Replace(original: List[Text], replacement: List[Text])

object CasualDiff:
  def parse(stream: LazyList[Text]): CasualDiff throws CasualDiffError =
    def recur
        (stream: LazyList[Text], original: List[Text], replacement: List[Text], done: List[Replace],
            line: Int)
        : List[Replace] = stream match
      case head #:: tail =>
        if head.s.startsWith("+ ") then
          if original.isEmpty
          then throw CasualDiffError(CasualDiffError.Issue.InsertionWithoutDeletion, line)
          else recur(tail, original, head.s.drop(2).tt :: replacement, done, line + 1)
        else if head.s.startsWith("- ") then
          if !replacement.isEmpty
          then
            val replace = Replace(original.reverse, replacement.reverse)
            recur(tail, List(head.s.drop(2).tt), Nil, replace :: done, line + 1)
          else recur(tail, head.s.drop(2).tt :: original, Nil, done, line + 1)
        else throw CasualDiffError(CasualDiffError.Issue.BadLineStart(head), line)
      
      case _ =>
        (Replace(original.reverse, replacement.reverse) :: done).reverse

    CasualDiff(recur(stream, Nil, Nil, Nil, 1))

case class CasualDiff(replacements: List[Replace]):
  def apply(original: LazyList[Text]): LazyList[Text] throws CasualDiffError =
    def recur
        (stream: LazyList[Text], focus: List[Text], todo: List[Replace], lineNo: Int)
        : LazyList[Text] =
      todo match
        case Nil =>
          LazyList()
          
        case Replace(original, replacement) :: tail => focus match
          case Nil => tail match
            case Replace(next, _) :: tail =>
              val lineNo2 = lineNo + original.length + replacement.length
              replacement.to(LazyList) #::: recur(stream, next, todo.tail, lineNo2)
            
            case Nil =>
              replacement.to(LazyList) #::: stream
  
          case line :: rest => stream match
            case head #:: tail =>
              if head == line then recur(tail, rest, todo, lineNo)
              else original.dropRight(focus.length).to(LazyList) #::: head #::
                  recur(tail, original, todo, lineNo)
            
            case _ =>
              val lineNo2 = lineNo + original.length - focus.length
              throw CasualDiffError(CasualDiffError.Issue.DoesNotMatch(line), lineNo2)
    
    if replacements.isEmpty then original
    else recur(original, replacements.head.original, replacements, 1)

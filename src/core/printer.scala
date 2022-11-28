package cellulose

import rudiments.*
import gossamer.*

import java.io as ji

import language.experimental.captureChecking

object Printer:
  def print(out: ji.Writer, doc: Doc): Unit =
    
    def recur(node: Node, indent: Int): Unit = node match
      case Node(data, meta) =>
        meta.mm: meta =>
          for i <- 0 until meta.blank do out.write('\n')
          meta.comments.foreach: comment =>
            for i <- 0 until indent do out.write(' ')
            out.write("# ")
            out.write(comment.s)
            out.write('\n')
        data match
          case Data(key, children, layout, schema) =>
            for i <- 0 until indent do out.write(' ')
            out.write(key.s)
            
            schema match
              case Field(_, _) =>
                children.foreach:
                  case Node(Data(key, _, _, _), _) =>
                    out.write(' ')
                    out.write(key.s)
                  case _ => throw Mistake("Should never match")
                out.write('\n')
              case Struct(_, _) =>
                val ps = if layout.multiline then children.take(layout.params - 1) else children.take(layout.params)
                ps.foreach:
                  case Node(Data(key, _, _, _), _) =>
                    out.write(' ')
                    out.write(key.s)
                  case _ =>
                    throw Mistake("Should never match")
                
                meta.mm(_.remark).mm: remark =>
                  out.write(" # ")
                  out.write(remark.s)
                
                if layout.multiline then
                  out.write('\n')
                  if children.length >= layout.params then children(layout.params - 1) match
                    case Node(Data(key, _, _, _), _) =>
                      for i <- 0 until (indent + 4) do out.write(' ')
                      for ch <- key.chars do
                        out.write(ch)
                        if ch == '\n' then for i <- 0 until (indent + 4) do out.write(' ')
                    case _ => throw Mistake("Should never match")
  
                out.write('\n')
                children.drop(layout.params).foreach(recur(_, indent + 2))
          case Unset => throw Mistake("Should never match")
    doc.children.foreach(recur(_, doc.margin))
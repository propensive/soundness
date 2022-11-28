package cellulose

import java.io as ji
import rudiments.*
import gossamer.*
import eucalyptus.*

import language.experimental.captureChecking

object Bin:
  private def write(out: ji.Writer, number: Int): Unit = out.write((number + 32).toChar)

  private def write(out: ji.Writer, text: Text): Unit =
    write(out, text.length)
    out.write(text.s)

  def write(out: ji.Writer, doc: Doc): Unit =
    out.write("\u00b1\u00c0\u00d1")
    write(out, doc.schema, doc.children)

  private def write(out: ji.Writer, schema: Schema, nodes: IArray[Node]): Unit =
    val dataNodes = nodes.map(_.data).sift[Data]
    write(out, dataNodes.length)
    
    dataNodes.foreach:
      case Data(key, children, _, _) =>
        schema match
          case Field(_, _) =>
            write(out, key)
          case _ =>
            val idx: Int = schema.keyMap.get(key).fold(0)(_ + 1)
            write(out, idx)
            write(out, schema.entry(idx - 1).schema, children)
      
  def read(schema: Schema, reader: ji.Reader): Doc throws BinaryError =
    if reader.read() != '\u00b1' || reader.read() != '\u00c0' || reader.read() != '\u00d1'
    then throw BinaryError(t"header 0xb1c0d1", 0)
    
    def recur(schema: Schema): List[Node] =
      List.range(0, readNumber(reader)).map: _ =>
        schema match
          case Field(_, _) =>
            val key = readText(reader)
            Node(Data(key, IArray(), Layout.empty, Schema.Free))
          
          case schema =>
            val subschema = readNumber(reader) match
              case 0  => (Unset, Schema.Free)
              case idx => schema.entry(idx - 1).tuple

            val key = subschema(0).option.getOrElse(throw BinaryError(t"unexpected key", 0))

            val children = IArray.from(recur(subschema(1)))
            
            Node(Data(key, children, Layout.empty, subschema(1)))
    
    Doc(IArray.from(recur(schema)), schema, 0)

  private def readNumber(in: ji.Reader): Int = in.read - 32

  private def readText(in: ji.Reader, length: Int = -1): Text =
    val buf = new Array[Char](if length == -1 then readNumber(in) else length)
    in.read(buf)
    String(buf).show
  
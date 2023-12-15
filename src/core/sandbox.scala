package superlunary

import jacinta.*, jsonPrinters.minimal
import anticipation.*
import spectacular.*
import guillotine.*
import turbulence.*
import rudiments.*
import ambience.*, systemProperties.jvm
import gossamer.*
import inimitable.*
import hieroglyph.*, charDecoders.utf8, charEncoders.utf8
import perforate.*
import eucalyptus.*
import hellenism.*

import scala.compiletime.*
import scala.quoted.*, staging.*

trait Container[+OutputType]


class Sandbox[InputType, OutputType](expr: Quotes ?=> Expr[InputType => OutputType]):

  def body(using Quotes): Expr[InputType => OutputType] = expr

// object Macros:
//   def container
//       [ThisType, InputType, OutputType]
//       (input: Expr[InputType])
//       (using Quotes, Type[InputType], Type[OutputType])(using thisType: Type[ThisType])
//       : Expr[OutputType] =
//     import quotes.reflect.*

    
//     val className = TypeRepr.of(using thisType).typeSymbol.fullName
//     val dest = Uuid().show
//     val settings: Compiler.Settings = Compiler.Settings.make(Some("/home/propensive/tmp/staging/"+dest), List(/*"-scalajs"*/))
//     given compiler: Compiler = Compiler.make(Macros.getClass.nn.getClassLoader.nn)(using settings)

//     TypeRepr.of(using thisType)

//     val sandbox = Class.forName(className).nn.getField("MODULE$").nn.get(null).asInstanceOf[Sandbox[InputType, OutputType]]

//     val body: Expr[InputType => OutputType] = sandbox.body

//     val serializer: Expr[JsonSerializer[InputType]] = Expr.summon[JsonSerializer[InputType]].get

//     val x = run:
//       '{
//         final class Abc():
//           var result: Any = 0
//           def main(args: Array[String]): Unit = ()
//             println($serializer)
//             //$body(???)
//       }

//     '{
//        val text: Text = $input.json(using $serializer).show
//        println(text)
//        $body($input)
//     }


inline def container
    [InputType: JsonSerializer: JsonDeserializer, OutputType]
    (body: Quotes ?=> Expr[InputType => OutputType])
    (input: InputType)
    : OutputType =

  val dest = Uuid().show
  val settings: Compiler.Settings = Compiler.Settings.make(Some("/home/propensive/tmp/staging/"+dest), List(/*"-scalajs"*/))
  given compiler: Compiler = Compiler.make(getClass.nn.getClassLoader.nn)(using settings)


  run: (quotes: Quotes) ?=>
    import quotes.reflect.*
    val entries: List[Text] = new Classloader(getClass.nn.getClassLoader.nn).classpath.or(???).entries.collect:
      case ClasspathEntry.Jarfile(file)        => file
      case ClasspathEntry.Directory(directory) => directory
    
    val classpath: Text = (entries :+ t"/home/propensive/tmp/staging/$dest").join(t":")
    val data = Expr(input.json.show)

    '{
      object Eex:
        def run(input: String): String =
          //val json: Json = safely(Json.parse(args(0).tt)).or(???)
          //val input: InputType = safely(json.as[InputType](using $deserializer)).or(???)
          unsafely: (raises) ?=>
            import hieroglyph.charEncoders.utf8
            val readable = summonInline[Readable[Text, Bytes]]
            val in = input.tt
            val json = Json.parse(in)(using readable, raises)
            val deserializer = summonInline[JsonDeserializer[InputType]]
            val param = json.as[InputType](using deserializer)

            val serializer = summonInline[JsonSerializer[OutputType]]

            MinimalSerializer.serialize($body(param).json(using serializer).root).s
      
      val cp: Text = ${Expr(classpath)}
      val d: Text = $data
      
      unsafely: raises ?=>
        import hieroglyph.charEncoders.utf8
        val wd: WorkingDirectory = workingDirectories.jvm
        val log: Log[Text] = logging.silent
        val cmd = Command(t"java", t"-classpath", cp, t"superlunary.SandboxRunner", t"Generated$$Code$$From$$Quoted$$Eex$$2$$", d)
        val output = cmd.exec[Text]()(using wd, log)

        val readable = summonInline[Readable[Text, Bytes]]
        val json = Json.parse(output)(using readable, raises)
        val deserializer = summonInline[JsonDeserializer[OutputType]]
        json.as[OutputType](using deserializer)
    }

object SandboxRunner:
  def main(args: Array[String]): Unit =
    val className = args(0)
    val params = args(1)
    val cls = Class.forName(className).nn
    val runnable = cls.newInstance()
    import scala.reflect.Selectable.reflectiveSelectable
    println(runnable.asInstanceOf[{ def run(params: String): String }].run(params))
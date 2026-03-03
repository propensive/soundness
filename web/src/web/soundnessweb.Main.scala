package dev.soundness

import soundness.*

import charDecoders.utf8
import charEncoders.utf8
import classloaders.scala
import logFormats.ansiStandard
import codicils.cancel
import environments.java
import termcaps.environment
import stdios.virtualMachine
import threading.platform
import httpServers.stdlibPublic

import html5.{Map => _, *}

given converter: HtmlTranslator()
given Realm = realm"soundness"
given Message is Loggable = safely(supervise(Log.route(Out))).or(Log.silent)
given Online = Online

def page(content: Html[Flow]*): HtmlDoc =
  HtmlDoc(Html
    ( Head
        ( Title(t"Soundness: The Direct-style Stack for Scala 3"),
          Meta(charset = enc"UTF-8"),
          Meta.Viewport(content = t"width=device-width, initial-scale=1"),
          // Script(async = true, src = t"https://www.googletagmanager.com/gtag/js?id=G-VH9CJTWYR3"),
          // Script(t"""window.dataLayer = window.dataLayer || [];
          //            function gtag(){dataLayer.push(arguments);}
          //            gtag('js', new Date());
          //            gtag('config', 'G-VH9CJTWYR3');
          //            """),
          Link.Stylesheet(href = % / "styles" / "soundness.css")),
        Body
          ( Main(content),
            Footer(P(t"© Copyright 2025 Jon Pretty")))) )

case class Reference(description: Text, link: Text)
case class Comment(id: Text, content: Text)
case class Event(date: Date, description: Text)

@main
def server(): Unit =
  recover:
    case AsyncError(reason) =>
      Out.println(m"There was a concurrency error")
      Exit.Fail(2).terminate()

    case ServerError(_) =>
      Out.println(m"Could not start server")
      Exit.Fail(2).terminate()

  . within:
      supervise(tcp"8080".serve[Http](handle))

class Service() extends JavaServlet(handle)

val modules: List[Text] = List("cli", "sci", "web", "test", "dev", "data")

val sections: List[Text] =
  List
    ( "scala3", "principles", "dependent", "exceptions", "composability", "nulls",
      "construction", "prose", "declarative", "decoupled", "polymorphic" )

def handle(using HttpConnection): Http.Response =
  recover:
    case error@PathError(path, reason) =>
      Http.Response(page(Aside, P(t"Path $path did not work")))

    case ClasspathError(path) =>
      Http.Response(page(Aside, H1(t"Path $path not found")))

    case UrlError(url, _, _) =>
      Http.Response(page(Aside, P(t"Invalid URL: $url")))

    case HostnameError(hostname, _) =>
      Http.Response(page(Aside, P(t"Invalid Hostname: $hostname")))

  . within:
      request.path match
        case _ /: t"styles" /: t"soundness.css" =>
          Http.Response(Classpath / "styles" / "soundness.css")

        case _ /: t"images" /: (image: Text) =>
          safely(Classpath / "images" / image).let: path =>
            Http.Response(path)
          . or:
              Http.Response(page(H1(t"Error")))

        case _ =>
          Http.Response
            ( page
                ( Img(src = t"/images/logo.svg"),
                  H1("The direct-style stack for Scala"),
                  H2
                    ( t"elegant by design ",
                      html5.Em(t"/"),
                      t" typesafe by definition ",
                      html5.Em(t"/"),
                      t" sound by default" ),
                  P(t"""Soundness is an ecosystem of Scala libraries that constrains your programs
                        to """, html5.Em(t"correctness"), t""" liberating you from the pain of runtime
                        errors, without compromising the legibility of your code."""),
                  P(t"""Your code feels just as intuitive as JavaScript or Python, but carries
                        the safety of Haskell or Rust."""),
                  Div.modules
                    ( modules.map: module =>
                        Div(Img(src = t"/images/$module.svg"), Code(module)) ),
                  Section
                    ( H3(t"Next Generation Scala"),
                      P(t"""Soundness was developed natively for Scala 3, and exploits its extensive new
                            features to the limit. Its sound typesystem and powerful metaprogramming
                            capabilities make it uniquely able to accommodate APIs that take full
                            advantage of precise types without compromising your code's legibility."""),
                      P(t"""Soundness is evolved from the experience of Scala 2, but not held back by its legacy. No
                            interface has been compromised in its expressiveness for compatibility with
                            Scala 2. Adopting Soundness means a full-throated endorsement of the future of
                            Scala.""")),
                  Section
                    ( H3(t"Two Golden Principles"),
                      P(t"""At the foundation of Soundness lie two core principles: Impossible states
                            should be unrepresentable, and transitions between states should be total.
                            Together, they ensure that your program starts in a correct state and never
                            leaves it. That means no unexpected runtime errors."""),
                      P(t"""Every library in the Soundness ecosystem is built on these principles, and
                            coerces your code along the path of correctness.""")),
                  Section
                    ( H3(t"Dependent Typing"),
                      P(t"""Soundness takes the idea that any static analysis a programmer can
                            do, the compiler can do better. And any static analysis the compiler """, html5.Em(t"can"), t"""
                            do, it """, html5.Em(t"should"), t""" do. So Soundness makes the Scala compiler do """, html5.Em(t"more"), t""" at
                            compiletime so you don't have to, and so the runtime doesn't have to. Scala's
                            powerful type system makes it possible to track more invariants about values
                            at compiletime, and to use that information to your advantage."""),
                      P(t"""Why write the code to handle a failure, if the source code has everything to
                            show it's impossible? With Soundness, you don't have to.""")),
                  Section
                    ( H3(t"Safe Exceptions"),
                      P(t"""Exceptional states are still valid states. Soundness will force you to handle
                            them to guarantee your program stays in a valid state. But only if you choose to!
                            If you just want to build a quick prototype, you can accept the risk of
                            runtime failure and focus on the happy path."""),
                      P(t"""Then come back later and make it safe—totally safe. And do it incrementally,
                            without major refactoring, and with the compiler's guidance every step of the
                            way.""")),
                  Section
                    ( H3(t"Composability"),
                      P(t"""Monads are the foundation of functional programming, but they don't compose. Lifting every
                            value into a monadic wrapper type is a burden that's familiar, but can never compose as easily as
                            expressions."""),
                      P(t"""Soundness APIs are """, html5.Em(t"direct-style"), t""" APIs. They are
                            designed to compose. And they're ready for a whole new level of safety with
                            Scala's advanced """, html5.Em(t"capture checking"), t""" functionality.""")),
                  Section
                    ( H3(t"Null Safety"),
                      P(t"The curse of ", Code(t"null"), ", the ", html5.Em(t"billion-dollar mistake"),
                            t" is well known by anyone who has ever encountered a ",
                            Code(t"NullPointerException"), t". In Soundness, ",
                            Code(t"null"),
                        t""" is unrepresentable and unnecessary, and Scala's type system enforces it. You
                            won't see a """, Code(t"null"), t" in Soundness code, and ",
                            Code(t"NullPointerException"), t"""s rarely ever
                            occur, so you can code without concern for them.""")),
                  Section
                    ( H3(t"Safe by Construction"),
                      P("""
                        Soundness makes it impossible to represent impossible states, so values are not
                        only known to be safe by construction, but checked at compiletime. When there's
                        not enough information available to check a value at compiletime, it's checked
                        at runtime—but you must define what happens if it's invalid."""),
                      P("""
                        Values like URLs, timestamps and ports are all eagerly verified, and they're
                        represented as literals. If they're not valid, it's a compile error.
                        Stringly-typed values—parsed at runtime—are a thing of the past.""")),
                  Section
                    ( H3(t"Elegant Prose"),
                      P(t"Good code reads like elegant prose."),
                      P(t"""
                        Just as functional programming introduced new nomenclature for the most common
                        code structures, Soundness has its own. But it's borrowed from English, so you
                        get a headstart understanding it. Familiar words make familiar appearances in
                        Soundness code, and each has been chosen with care for its clarity, uniqueness,
                        nuance and expressiveness.""")),
                  Section
                    ( H3(t"Declarative Programming"),
                      P("""
                        Declarative code is easier to reason about because it's independent of control flow. Its essential
                        structure arises from scopes and contexts, and in Scala 3 it's possible with """, html5.Em(t"contextual values"), t""".
                        Declare a new """, Code(t"given"), t""" instance or import an existing one, and it's valid for the entire scope. And
                        you decide whether that's just a method body, or your entire project—on a continuum between local
                        and global. There's less repetition, and your code is more maintainable.""")),
                  Section
                    ( H3(t"Decoupled Integration"),
                      P(t"""
                        Soundness is a vast ecosystem of small libraries, for many diverse applications. For specific needs,
                        individual libraries can be selected à la carte, without introducing a complex graph of
                        dependencies. Care has been taken to decouple libraries which aren't directly related, without
                        compromising their integration. Soundness's typeclass-based approach has made it possible to avoid
                        unnecessary dependencies through the careful specification of small interfaces."""),
                      P(t"""
                        But Soundness also provides six bundles of libraries targeting different domains. These are """, Code(t"web"), t"""
                        for web applications; """, Code(t"data"), t""" for data processing; """, Code(t"sci"), t""" for scientific applications; """, Code(t"cli"), t""" for
                        command-line applications; """, Code(t"test"), t""" for testing; and, """, Code(t"tool"), t""" for tooling development. The """, Code(t"base"), t"""
                        bundle provides a common set of fundamental libraries, and """, Code(t"all"), t""" includes everything.""")),
                  Section
                    ( H3(t"Polymorphic and Small"),
                      P(t"""
                        For all the functionality it provides, Soundness's API is
                        """, html5.Em(t"tiny"), t""". Instead of a multitude of similar methods and types,
                        and without encoding functionality in
                        method names, Soundness prefers fewer methods and fewer types,
                        but each more versatile and composable. Types can be composed
                        in a variety of ways, with the resultant type exhibiting
                        the properties of its constituent parts in predictable ways, without duplicating functionality."""))) )

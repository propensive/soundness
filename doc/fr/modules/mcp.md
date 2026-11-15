- ## MCP
+ ## MCP

- ### About
+ ### Présentation

- The [Model Context Protocol](https://modelcontextprotocol.io/) is how an AI assistant reaches
- outside itself, by calling tools, reading resources and using prompts which a server provides.
+ Le [Model Context Protocol](https://modelcontextprotocol.io/) est la façon dont un assistant
+ d'IA s'étend au-delà de lui-même, en appelant des outils, en lisant des ressources et en
+ utilisant des prompts que fournit un serveur.

- An MCP server here is an object with annotated methods. A method marked `@tool` becomes a
- callable tool, and its parameters and result are described to the client by
- [JSON schemas](json.md#schemas) derived from their types. `@resource` exposes content at a URI.
- `@prompt` supplies a conversation template. The protocol itself, which is JSON-RPC over
- streamable HTTP, with sessions and listings, is generated from those definitions.
+ Un serveur MCP est ici un objet doté de méthodes annotées. Une méthode marquée `@tool` devient un
+ outil appelable, et ses paramètres et son résultat sont décrits au client par des
+ [schémas JSON](json.md#schemas) dérivés de leurs types. `@resource` expose un contenu à une URI.
+ `@prompt` fournit un modèle de conversation. Le protocole lui-même, du JSON-RPC sur HTTP en
+ flux, avec sessions et listes, est généré à partir de ces définitions.

- ### On MCP
+ ### MCP

- An MCP server describes itself. Tools are advertised with their names, descriptions and input
- schemas, and the client, which is the harness of a language model, decides what to call from
- those descriptions. Writing that self-description by hand duplicates the code it describes, and
- the two drift. The schema says one thing and the implementation another, and the model's tool
- calls fail in ways it cannot see.
+ Un serveur MCP se décrit lui-même. Les outils sont annoncés avec leurs noms, leurs descriptions
+ et leurs schémas d'entrée, et le client, qui est le harnais d'un modèle de langage, décide quoi
+ appeler d'après ces descriptions. Écrire cette autodescription à la main duplique le code qu'elle
+ décrit, et les deux divergent. Le schéma dit une chose et l'implémentation une autre, et les
+ appels d'outils du modèle échouent d'une façon qu'il ne peut pas voir.

- Deriving the description from the implementation removes the gap. The method *is* the tool. Its
- name, its parameter types and its result type generate the listing, and dispatch goes straight to
- it. The schema cannot disagree with the code, which is
- [correctness](../../philosophy/correctness.md) by construction.
+ Dériver la description de l'implémentation supprime l'écart. La méthode *est* l'outil. Son nom,
+ les types de ses paramètres et son type de résultat génèrent la liste, et la répartition va
+ directement à elle. Le schéma ne peut pas être en désaccord avec le code : c'est la
+ [justesse](../../philosophy/correctness.md) par construction.

- Everything comes from the `soundness` package:
+ Tout provient du package `soundness` :

```scala
import soundness.*

import classloaders.threadContextClassloader
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import threads.virtualThreads
import webserverErrorPages.minimalErrorPage
```

- The samples on this page serve a protocol over HTTP, so they are compiled but not run.
+ Les exemples de cette page servent un protocole sur HTTP : ils sont donc compilés mais pas
+ exécutés.

- ### A server
+ ### Un serveur

- A server extends `Mcp.Server`, names itself, and defines its capabilities as annotated methods.
- Each connected client gets a `Session`, created by `initialize`, in which the server may keep
- state for that client:
+ Un serveur étend `Mcp.Server`, se nomme, et définit ses capacités sous forme de méthodes
+ annotées. Chaque client connecté reçoit une `Session`, créée par `initialize`, dans laquelle le
+ serveur peut conserver un état propre à ce client :

```scala
val colors = Map(t"sky" -> t"blue", t"grass" -> t"green")

object AssistantTools extends Mcp.Server():
  class Session() extends Mcp.Session
  def initialize(): Session = new Session()

  def name: Text = t"assistant-tools"
  def description: Text = t"Utilities for the assistant"
  def version: Semver = v"1.0.0"
  def prompts: List[Mcp.Prompt] = Nil

  @tool
  @about("Look up the color of a named thing")
  def color(name: Text): Text = colors.at(name).or(t"unknown")
```

- The input schema of the tool, a `name` of type string, and its result schema are derived from
- the signature. `@about` supplies the description which the model reads when it decides whether
- to call the tool. These are the annotations which the methods of a server may carry:
+ Le schéma d'entrée de l'outil, un `name` de type chaîne, et son schéma de résultat sont dérivés
+ de la signature. `@about` fournit la description que lit le modèle lorsqu'il décide d'appeler ou
+ non l'outil. Voici les annotations que peuvent porter les méthodes d'un serveur :

```tabular
- column  Annotation
- column  On
- column  Makes it
- row  `@tool`  a method  a callable tool
- row  `@resource(uri)`  a method  content served at the URI
- row  `@prompt`  a method  a conversation template
- row  `@about(text)`  a method or a parameter  described, for the model
- row  `@title(text)`  a method  titled, for display
- row  `@ui(uri)`  a tool  a tool whose user interface is the resource at the URI
+ column  Annotation
+ column  Sur
+ column  En fait
+ row  `@tool`  une méthode  un outil appelable
+ row  `@resource(uri)`  une méthode  un contenu servi à l'URI
+ row  `@prompt`  une méthode  un modèle de conversation
+ row  `@about(text)`  une méthode ou un paramètre  un élément décrit, pour le modèle
+ row  `@title(text)`  une méthode  un élément titré, pour l'affichage
+ row  `@ui(uri)`  un outil  un outil dont l'interface utilisateur est la ressource à l'URI
```

- ### Talking back to the client
+ ### Répondre au client

- A tool which takes a `using Mcp.Client` can speak to the client while it runs. `log` streams
- progress notifications. `elicit` asks the user for structured input in the middle of a call,
- with its form described by a derived schema:
+ Un outil qui prend un `using Mcp.Client` peut parler au client pendant son exécution. `log`
+ diffuse des notifications de progression. `elicit` demande à l'utilisateur une saisie structurée
+ au milieu d'un appel, dont le formulaire est décrit par un schéma dérivé :

```scala
object Deployer extends Mcp.Server():
  class Session() extends Mcp.Session
  def initialize(): Session = new Session()
  def name: Text = t"deployer"
  def description: Text = t"Deploys the application"
  def version: Semver = v"1.0.0"
  def prompts: List[Mcp.Prompt] = Nil

  @tool
  def deploy(target: Text)(using client: Mcp.Client): Text =
    client.log(t"Preparing deployment to $target")
    case class Confirmation(proceed: Boolean)
    client.elicit[Confirmation](t"Deploy to production?")
    t"deployed to $target"
```

- A parameter is required unless its type is an [`Optional`](optional-values.md), or it has a
- default argument. A client may omit those, and the tool sees `Unset` or the default. An `@about`
- on a parameter describes it in the input schema of the tool:
+ Un paramètre est obligatoire à moins que son type ne soit un
+ [`Optional`](optional-values.md), ou qu'il n'ait un argument par défaut. Un client peut omettre
+ ceux-là, et l'outil voit `Unset` ou la valeur par défaut. Un `@about` sur un paramètre le décrit
+ dans le schéma d'entrée de l'outil :

```scala
@tool
@about(t"Lists recent runs")
def runs(@about(t"only runs in this workspace") workspace: Optional[Text], limit: Int = 20)
:   List[Text] =
  Nil
```

- A failure inside a tool is something the model can read and act on. A malformed call is not:
+ Un échec au sein d'un outil est quelque chose que le modèle peut lire et sur quoi il peut agir.
+ Un appel mal formé ne l'est pas :

```tabular
- column  When
- column  The client receives
- row  a tool throws  a result marked `isError`, carrying the exception's message
- row  an unknown tool is called  a JSON-RPC error
- row  a required parameter is omitted  a JSON-RPC error
+ column  Quand
+ column  Le client reçoit
+ row  un outil lève une exception  un résultat marqué `isError`, portant le message de l'exception
+ row  un outil inconnu est appelé  une erreur JSON-RPC
+ row  un paramètre obligatoire est omis  une erreur JSON-RPC
```

- ### Resources and prompts
+ ### Ressources et prompts

- `@resource` serves content at a URI. The content is anything which is streamable as text or
- bytes. `@prompt` returns a conversation, written with the `human` and `agent` interpolators which
- [LLM](llm.md) describes. The MIME type of a resource may be given with the annotation's
- `mimeType`, and is otherwise chosen for it:
+ `@resource` sert un contenu à une URI. Le contenu est tout ce qui peut être diffusé sous forme de
+ texte ou d'octets. `@prompt` renvoie une conversation, écrite avec les interpolateurs `human` et
+ `agent` que décrit [LLM](llm.md). Le type MIME d'une ressource peut être donné avec le
+ `mimeType` de l'annotation, et est sinon choisi pour elle :

```tabular
- column  A resource
- column  Has the MIME type
- row  named by a tool's `@ui`  `text/html;profile=mcp-app`
- row  streamable as text  `text/plain`
- row  streamable only as bytes  `application/octet-stream`
+ column  Une ressource
+ column  A le type MIME
+ row  nommée par le `@ui` d'un outil  `text/html;profile=mcp-app`
+ row  diffusable sous forme de texte  `text/plain`
+ row  diffusable seulement sous forme d'octets  `application/octet-stream`
```

```scala
object Documentation extends Mcp.Server():
  class Session() extends Mcp.Session
  def initialize(): Session = new Session()
  def name: Text = t"documentation"
  def description: Text = t"The project's documentation"
  def version: Semver = v"1.0.0"
  def prompts: List[Mcp.Prompt] = Nil

  @resource("docs://readme", mimeType = t"text/markdown")
  @about("The project readme")
  def readme: Text = t"# Readme\n\nStart with `make build`."

  @prompt
  def review(topic: Text): List[Discourse] =
    List(human"Please review the $topic and summarize any problems.")
```

- ### Serving
+ ### Servir

- The server speaks the streamable HTTP transport of MCP: JSON-RPC requests by POST, an event
- stream by GET, and sessions tracked by a header. It mounts inside an ordinary
- [HTTP server](http-server.md) handler:
+ Le serveur parle le transport HTTP en flux de MCP : des requêtes JSON-RPC par POST, un flux
+ d'événements par GET, et des sessions suivies par un en-tête. Il se monte dans un gestionnaire
+ de [serveur HTTP](http-server.md) ordinaire :

```scala
def serve(): Unit = supervise:
  SocketServer(8080).handle:
    request.path match
      case % /: t"mcp" => AssistantTools.serve
      case _           => Http.Response(Http.NotFound)(t"Not found")
```

- A client configured with that endpoint discovers the tools, resources and prompts. Everything it
- sees was derived from the methods it will call.
+ Un client configuré avec cet endpoint découvre les outils, les ressources et les prompts. Tout ce
+ qu'il voit a été dérivé des méthodes qu'il appellera.

- ### Related
+ ### Voir aussi

- - [ACP](acp.md) is the sibling protocol, for an editor to drive an agent.
- - [LLM](llm.md) describes the `human` and `agent` interpolators, and the models which call
-   these tools.
- - [RPC](rpc.md) describes the JSON-RPC and server-sent events which the transport uses.
- - [HTTP server](http-server.md) hosts the endpoint.
- - [JSON](json.md#schemas) describes the schemas which describe each tool.
- - [Annotations](annotations.md) describes how annotations are read at compiletime.
+ - [ACP](acp.md) est le protocole jumeau, qui permet à un éditeur de piloter un agent.
+ - [LLM](llm.md) décrit les interpolateurs `human` et `agent`, et les modèles qui appellent ces
+   outils.
+ - [RPC](rpc.md) décrit le JSON-RPC et les server-sent events qu'utilise le transport.
+ - [Serveur HTTP](http-server.md) héberge l'endpoint.
+ - [JSON](json.md#schemas) décrit les schémas qui décrivent chaque outil.
+ - [Annotations](annotations.md) décrit comment les annotations sont lues à la compilation.

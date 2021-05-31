package acyclicity

import contextual.*

import language.dynamics

case class Id(key: String) extends Dynamic:
  def applyDynamicNamed(method: "apply")(attrs: (String, String)*) =
    Statement.Node(this, attrs.map { case (k, v) => Attribute(k, v) }*)
  
  def :=(id: Id): Statement.Assign = Statement.Assign(this, id)

case class CompassId(id: Id, compass: Option[Compass] = None)
case class NodeId(id: Id, port: Option[CompassId] = None):
  def --(dest: NodeId | Statement.Subgraph): Statement.Edge = Statement.Edge(this, Target(false, dest, None))
  def -->(dest: NodeId | Statement.Subgraph): Statement.Edge = Statement.Edge(this, Target(true, dest, None))

enum Compass:
  case North, South, East, West, NorthEast, NorthWest, SouthEast, SouthWest

enum Dot:
  case Graph(id: Option[Id], strict: Boolean, statements: Statement*)
  case Digraph(id: Option[Id], strict: Boolean, statements: Statement*)

  def serialize: String = Dot.serialize(Dot.tokenize(this))

enum Statement:
  case Node(id: Id, attrs: Attribute*)
  case Edge(id: NodeId, rhs: Target, attrs: Attribute*)
  case Assign(id: Id, id2: Id)
  case Subgraph(id: Option[Id], statements: Statement*)

case class Target(directed: Boolean, dest: NodeId | Statement.Subgraph, link: Option[Target])

case class Attribute(key: String, value: String)

object Dot:
  def serialize(tokens: Vector[String]): String =
    var buf: StringBuilder = StringBuilder()
    var level: Int = 0
    var end: Boolean = true

    def indent(): Unit = level += 1
    def outdent(): Unit = level -= 1
    def newline(): Unit = end = true

    def whitespace(): Unit =
      if end then
        buf.append("\n")
        buf.append("  "*level)
        end = false
      else buf.append(" ")

    tokens.foreach {
      case ""   => ()
      case ","  => buf.append(",")
      case "{"  => whitespace(); buf.append("{"); indent(); newline()
      case "}"  => outdent(); whitespace(); buf.append("}"); newline()
      case "["  => whitespace(); buf.append("[")
      case "]"  => whitespace(); buf.append("]"); newline()
      case ";"  => newline()
      case word => whitespace(); buf.append(word)
    }

    buf.toString

  def tokenize(graph: NodeId | Dot | Target | Statement | Attribute): Vector[String] = graph match
    case NodeId(id, port) =>
      Vector(port.fold(id.key) { p => s"${id.key}:$p" })
    
    case Attribute(key, value) =>
      Vector(s"""$key="$value"""")
    
    case Target(directed, dest, link) =>
      val op = if directed then "->" else "--"
      Vector(op) ++ tokenize(dest) ++ link.to(Vector).flatMap(tokenize(_)) ++ Vector(";")

    case Statement.Node(id, attrs*) =>
      Vector(s""""${id.key}"""") ++ (if attrs.isEmpty then Vector() else (Vector("[") ++
          attrs.to(Vector).flatMap(tokenize(_) :+ ",").init ++ Vector("]", ";")))
    
    case Statement.Edge(id, rhs, attrs*) =>
      tokenize(id) ++ tokenize(rhs)
    
    case Statement.Assign(id, id2) =>
      Vector(id.key, "=", id2.key, ";")
    
    case Statement.Subgraph(id, statements*) =>
      Vector("subgraph") ++ id.to(Vector).map(_.key) ++ Vector("{") ++ statements.flatMap(tokenize(_)) ++
          Vector("}")
    
    case Dot.Graph(id, strict, statements*) =>
      Vector(
        if strict then Vector("strict") else Vector(), Vector("graph"), id.to(Vector).map(_.key), Vector("{"),
            statements.flatMap(tokenize(_)), Vector("}")
      ).flatten
    
    case Dot.Digraph(id, strict, statements*) =>
      Vector(
        if strict then Vector("strict") else Vector(), Vector("digraph"), id.to(Vector).map(_.key), Vector("{"),
            statements.flatMap(tokenize(_)), Vector("}")
      ).flatten

object Digraph:
  def apply(id: Id, statements: Statement*): Dot = Dot.Digraph(Some(id), false, statements*)
  def strict(id: Id, statements: Statement*): Dot = Dot.Digraph(Some(id), true, statements*)

object Graph:
  def apply(id: Id, statements: Statement*): Dot = Dot.Graph(Some(id), false, statements*)
  def strict(id: Id, statements: Statement*): Dot = Dot.Graph(Some(id), true, statements*)

object Subgraph:
  def apply(id: Id)(statements: Statement*): Statement.Subgraph = Statement.Subgraph(Some(id), statements*)
  def apply(statements: Statement*): Statement.Subgraph = Statement.Subgraph(None, statements*)

object NodeParser extends Interpolator[Unit, Option[NodeId], NodeId]:
  def parse(state: Option[NodeId], next: String): Some[NodeId] = Some { next.split(":").to(List) match
    case List(id) =>
      NodeId(Id(id))
    
    case List(id, port) =>
      NodeId(Id(id), Some(CompassId(Id(port))))
    
    case List(id, port, compass@("n" | "e" | "s" | "w" | "ne" | "nw" | "se" | "sw")) =>
      NodeId(Id(id), Some(CompassId(Id(port), Some(Compass.valueOf(compass)))))
    
    case _ =>
      throw ParseError("not a valid node ID")
  }
  
  def initial: Option[NodeId] = None
  def complete(value: Option[NodeId]): NodeId = value.get
  def insert(state: Option[NodeId], value: Option[Unit]): Option[NodeId] = state

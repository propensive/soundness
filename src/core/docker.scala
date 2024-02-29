package embarcadero

import anticipation.*
import telekinesis.*
import jacinta.*

case class ContainerImage(id: Text)

case class Container(id: Text)

case class DockerEngine(port: Int)

@main
def run(): Unit =
  url"http://localhost/images/json"
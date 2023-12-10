package wharfinger

import anticipation.*
import telekinesis.*
import jacinta.*

case class ContainerImage(id: Text)

case class Container(id: Text)

case class DockerEngine()

@main
def run(): Unit =
  url"http://localhost/images/json"
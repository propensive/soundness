package cellulose

import nettlesome.*
import anticipation.*
import gossamer.*
import spectacular.*
import contingency.*, errorHandlers.throwUnsafely
import turbulence.*, stdioSources.virtualMachine

case class Network(address: MacAddress)

@main def run(): Unit =

  val data: Text = t"""
    address   78-ab-c1-ff-01-23
  """

  val network = Codl.read[Network](data)
  summon[Debug[Network]]
  Out.println(t"${network.debug}")
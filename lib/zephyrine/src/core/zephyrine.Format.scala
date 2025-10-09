package zephyrine

import anticipation.*
import fulminate.*

object Format:
  trait Position:
    def describe: Text

  trait Issue:
    def describe: Message

trait Format:
  def name: Text
  type Position <: Format.Position
  type Issue <: Format.Issue

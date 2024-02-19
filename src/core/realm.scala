package fulminate

import anticipation.*

import scala.quoted.*

object Realm:
  def make(name: Text): Realm = new Realm(name)

case class Realm(name: Text)

object Fulminate:
  def realm(context: Expr[StringContext])(using Quotes): Expr[Realm] =
    import quotes.reflect.*
    val name: String = context.valueOrAbort.parts.head
    if !name.matches("[a-z]+") then fail(msg"the realm name should comprise only of lowercase letters")
    else '{Realm.make(${Expr(name)}.tt)}

extension (inline context: StringContext)
  inline def realm(): Realm = ${Fulminate.realm('context)}

package hypotenuse

import fulminate.*

import scala.quoted.*

import java.lang.{Integer as JInt, Long as JLong, Short as JShort, Byte as JByte, Double as JDouble,
    Float as JFloat}

import language.experimental.genericNumberLiterals

object Hypotenuse2:
  def parseU64(digits: Expr[String])(using Quotes): Expr[Long] = digits.value match
    case None         => '{JLong.parseUnsignedLong($digits)}
    case Some(digits) => Expr(JLong.parseUnsignedLong(digits))

  def parseI64(digits: Expr[String])(using Quotes): Expr[Long] = digits.value match
    case None         => '{JLong.parseLong($digits)}
    case Some(digits) => Expr(JLong.parseLong(digits))
  
  def parseU32(digits: Expr[String])(using Quotes): Expr[Int] = digits.value match
    case None         => '{JInt.parseUnsignedInt($digits)}
    case Some(digits) => Expr(JInt.parseUnsignedInt(digits))

  def parseI32(digits: Expr[String])(using Quotes): Expr[Int] = digits.value match
    case None         => '{JInt.parseInt($digits)}
    case Some(digits) => Expr(JInt.parseInt(digits))
  
  def parseU16(digits: Expr[String])(using Quotes): Expr[Short] = digits.value match
    case None => '{JInt.parseInt($digits).toShort}
    
    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < 0 then fail(msg"a U16 may not be less than ${0}")
      if int > 0xffff then fail(msg"a U16 may not be greater than ${0xffff}")
      
      Expr(int.toShort)

  def parseI16(digits: Expr[String])(using Quotes): Expr[Short] = digits.value match
    case None => '{JInt.parseInt($digits).toShort}
    
    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < Short.MinValue then fail(msg"an I16 may not be less than ${Short.MinValue.toInt}")
      if int > Short.MaxValue then fail(msg"an I16 may not be greater than ${Short.MaxValue.toInt}")
      
      Expr(int.toShort)
  
  def parseU8(digits: Expr[String])(using Quotes): Expr[Byte] = digits.value match
    case None => '{JInt.parseInt($digits).toByte}
    
    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < 0 then fail(msg"a U8 may not be less than ${0}")
      if int > 0xffff then fail(msg"a U8 may not be greater than ${0xffff}")
      
      Expr(int.toByte)

  def parseI8(digits: Expr[String])(using Quotes): Expr[Byte] = digits.value match
    case None => '{JInt.parseInt($digits).toByte}
    
    case Some(digits) =>
      val int = JInt.parseInt(digits)
      if int < Byte.MinValue then fail(msg"an I8 may not be less than ${Byte.MinValue.toInt}")
      if int > Byte.MaxValue then fail(msg"an I8 may not be greater than ${Byte.MaxValue.toInt}")
      
      Expr(int.toByte)
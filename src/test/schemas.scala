package cellulose

import gossamer.*
import rudiments.*
import eucalyptus.*
import parasitism.*, threading.platform
import logging.silent

import Arity.*

val Greek = Struct(Optional,
  t"alpha" -> Field(Optional),
  t"beta"  -> Field(One),
  t"gamma" -> Field(Many),
  t"delta" -> Field(AtLeastOne),
  t"eta"   -> Field(Unique),
  t"iota"  -> Field(Optional),
  t"kappa" -> Field(Many)
)

object GreekRecords extends RecordType(Greek):
  transparent inline def record(inline fn: String => Any): polyvinyl.Record = ${build('fn)}

val example1 = unsafely:
  Greek.parse(t"""
    alpha  one
    beta   two
    gamma  three four five
    delta  six seven
    eta    eight
    kappa  nine
    kappa  ten eleven
  """)
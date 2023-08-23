package fluorescent

import probably.*
import gossamer.*
import gastronomy.*, hashFunctions.sha1
import anticipation.*

import language.experimental.genericNumberLiterals

object Tests extends Suite(t"Fluorescent tests"):
  def run(): Unit =
    test(t"Check how many bits are required for a bloom filter"):
      val bloom = BloomFilter[Text](100, 0.01)
      bloom.bitSize
    .assert(_ == 663)
    
    test(t"Check that more bits are required to store more elements"):
      val bloom = BloomFilter[Text](1000, 0.01)
      bloom.bitSize
    .assert(_ == 6631)
    
    test(t"Check that more bits are required to store with more certainty"):
      val bloom = BloomFilter[Text](100, 0.001)
      bloom.bitSize
    .assert(_ == 994)

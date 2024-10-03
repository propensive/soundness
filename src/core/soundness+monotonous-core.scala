package soundness

export monotonous.{Alphabet, Base32, Base64, Binary, Deserializable, Hex, Octal, Quaternary,
    Serializable, Serialization, SerializationError, deserialize, serialize}

package alphabets:
  package binary:
    export monotonous.alphabets.binary.standard

  package quaternary:
    export monotonous.alphabets.quaternary.{standard, dnaNucleotide}
  
  package octal:
    export monotonous.alphabets.octal.standard
  
  package hex:
    export monotonous.alphabets.hex.{strictUpperCase, strictLowerCase, upperCase, lowerCase,
        bioctal}

  package base32:
    export monotonous.alphabets.base32.{strictUpperCase, strictLowerCase, upperCase, lowerCase,
        extendedHexUpperCase, extendedHexLowerCase, zBase32, zBase32Unpadded, geohash, wordSafe,
        crockford}
  
  package base64:
    export monotonous.alphabets.base64.{standard, unpadded, url, xml, imap, yui, radix64, bcrypt,
        sasl, uuencoding}


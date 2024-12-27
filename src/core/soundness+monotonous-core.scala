/*
    Monotonous, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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


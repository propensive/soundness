                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package crossparse

import ambience.*, systems.javaSystem
import anticipation.*
import breviloquence.*
import contingency.*, strategies.throwUnsafely
import distillate.*
import fulminate.*
import gossamer.*
import jacinta.*
import locomotion.*
import prepositional.*
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import spectacular.*
import stratiform.*
import superlunary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import turbulence.*
import vacuous.*
import xylophone.*
import ypsiloid.*

import jacinta.formatting.compactJsonFormatting
import ypsiloid.formatting.blockYamlFormatting
import ypsiloid.showable

// ── The document matrix ─────────────────────────────────────────────────
// Typed translations of the jacinta benchmark corpus (see
// `crossparse.model`), each serialized once through every format's own
// encoder and decoded back two ways per format — through the materialized
// AST and through the inlined parser (YAML AST-only) — so the comparison
// spans document shapes: a nested config, a small fragment, flat record
// tables, and numeric arrays. One object per document, so each classfile
// stays within the JVM's method and constant-pool limits.

object matrixConfig:
  given schema: XmlSchema = XmlSchema.Freeform

  val corpus: Config = corpora.config()

  lazy val json: Data = corpus.in[Json].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val tel: Data = corpus.in[Tel].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val xml: Text = corpus.in[Xml].show
  lazy val yaml: Text = corpus.in[Yaml].show
  lazy val cbor: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(corpus.in[Cbor]))
  lazy val protobuf: Data = corpus.in[Protobuf].encode
  lazy val bintel: Data = corpus.bintel

  given inlinedJson: Config is Json.Parsable = jacinta.Inlinable.parsable[Config]
  given inlinedTel: Config is Tel.Parsable = stratiform.Inlinable.parsable[Config]
  given inlinedXml: Config is Xml.Parsable = xylophone.Inlinable.parsable[Config]
  given inlinedCbor: Config is Cbor.Parsable = breviloquence.Inlinable.parsable[Config]

  given inlinedProtobuf: Config is Protobuf.Parsable =
    locomotion.Inlinable.parsable[Config]

  given inlinedBintel: Config is Bintel.Parsable = BintelInlinable.parsable[Config]

  def jsonAst(): Config = json.read[Json].as[Config]
  def jsonInlined(): Config = json.read[Config in Json]
  def telAst(): Config = tel.read[Tel].as[Config]
  def telInlined(): Config = tel.read[Config in Tel]
  def xmlAst(): Config = xml.read[Xml].as[Config]
  def xmlInlined(): Config = xml.read[Config in Xml]
  def yamlAst(): Config = yaml.read[Yaml].as[Config]
  def cborAst(): Config = cbor.read[Cbor].as[Config]
  def cborInlined(): Config = cbor.read[Config in Cbor]
  def protobufAst(): Config = protobuf.read[Protobuf].as[Config]
  def protobufInlined(): Config = protobuf.read[Config in Protobuf]
  def bintelAst(): Config = Bintel.read[Config](bintel)
  def bintelInlined(): Config = Bintel.parse[Config](bintel)

  // ── The third-party baselines ──
  def jsoniterRival(): MConfig =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[MConfig]
      (json.mutable(using Unsafe))(using jsoniterCodecs.config)

  def borerRival(): MConfig =
    io.bullet.borer.Cbor.decode(cbor.mutable(using Unsafe)).to[MConfig]
      (using borerCodecs.config).value

  def protobufJavaRival(): MConfig = protobufWalks.config(protobuf)
  def aaltoRival(): MConfig = aaltoWalks.config(xml.s)
  def snakeyamlRival(): MConfig = snakeyamlWalks.config(yaml.s)

  // The correctness gate: every arm must reproduce the corpus before
  // anything is timed.
  def check(): Unit =
    assert(jsonInlined() == corpus, "config: JSON inlined decode disagrees")
    assert(jsonAst() == corpus, "config: JSON AST decode disagrees")
    assert(telInlined() == corpus, "config: TEL inlined decode disagrees")
    assert(telAst() == corpus, "config: TEL AST decode disagrees")
    assert(xmlInlined() == corpus, "config: XML inlined decode disagrees")
    assert(xmlAst() == corpus, "config: XML AST decode disagrees")
    assert(yamlAst() == corpus, "config: YAML AST decode disagrees")
    assert(cborInlined() == corpus, "config: CBOR inlined decode disagrees")
    assert(cborAst() == corpus, "config: CBOR AST decode disagrees")
    assert(protobufInlined() == corpus, "config: Protobuf inlined decode disagrees")
    assert(protobufAst() == corpus, "config: Protobuf AST decode disagrees")
    assert(bintelInlined() == corpus, "config: BinTEL inlined decode disagrees")
    assert(bintelAst() == corpus, "config: BinTEL AST decode disagrees")

    val mirror = mirrors.config(corpus)
    assert(jsoniterRival() == mirror, "config: Jsoniter decode disagrees")
    assert(borerRival() == mirror, "config: borer decode disagrees")
    assert(protobufJavaRival() == mirror, "config: protobuf-java decode disagrees")
    assert(aaltoRival() == mirror, "config: Aalto decode disagrees")
    assert(snakeyamlRival() == mirror, "config: snakeyaml decode disagrees")

  def suites[report](bench: Bench)
    (using Runner[report], Inclusion[report, Benchmark], Testable)
  :   Unit =

    suite(m"Decode the config corpus to case classes"):
      bench(m"JSON inlined")(target = 1*Second):
        '{ crossparse.matrixConfig.jsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.matrixConfig.jsonAst() }

      bench(m"Jsoniter (JSON)")(target = 1*Second):
        '{ crossparse.matrixConfig.jsoniterRival() }

      bench(m"TEL inlined")(target = 1*Second):
        '{ crossparse.matrixConfig.telInlined() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.matrixConfig.telAst() }

      bench(m"XML inlined")(target = 1*Second):
        '{ crossparse.matrixConfig.xmlInlined() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.matrixConfig.xmlAst() }

      bench(m"Aalto (XML)")(target = 1*Second):
        '{ crossparse.matrixConfig.aaltoRival() }

      bench(m"CBOR inlined")(target = 1*Second):
        '{ crossparse.matrixConfig.cborInlined() }

      bench(m"CBOR via AST")(target = 1*Second):
        '{ crossparse.matrixConfig.cborAst() }

      bench(m"borer (CBOR)")(target = 1*Second):
        '{ crossparse.matrixConfig.borerRival() }

      bench(m"Protobuf inlined")(target = 1*Second):
        '{ crossparse.matrixConfig.protobufInlined() }

      bench(m"Protobuf via AST")(target = 1*Second):
        '{ crossparse.matrixConfig.protobufAst() }

      bench(m"protobuf-java")(target = 1*Second):
        '{ crossparse.matrixConfig.protobufJavaRival() }

      bench(m"BinTEL inlined")(target = 1*Second):
        '{ crossparse.matrixConfig.bintelInlined() }

      bench(m"BinTEL via AST")(target = 1*Second):
        '{ crossparse.matrixConfig.bintelAst() }

      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.matrixConfig.yamlAst() }

      bench(m"SnakeYAML (YAML)")(target = 1*Second):
        '{ crossparse.matrixConfig.snakeyamlRival() }

object matrixMenu:
  given schema: XmlSchema = XmlSchema.Freeform

  val corpus: MenuDoc = corpora.menu()

  lazy val json: Data = corpus.in[Json].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val tel: Data = corpus.in[Tel].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val xml: Text = corpus.in[Xml].show
  lazy val yaml: Text = corpus.in[Yaml].show
  lazy val cbor: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(corpus.in[Cbor]))
  lazy val protobuf: Data = corpus.in[Protobuf].encode
  lazy val bintel: Data = corpus.bintel

  given inlinedJson: MenuDoc is Json.Parsable = jacinta.Inlinable.parsable[MenuDoc]
  given inlinedTel: MenuDoc is Tel.Parsable = stratiform.Inlinable.parsable[MenuDoc]
  given inlinedXml: MenuDoc is Xml.Parsable = xylophone.Inlinable.parsable[MenuDoc]
  given inlinedCbor: MenuDoc is Cbor.Parsable = breviloquence.Inlinable.parsable[MenuDoc]

  given inlinedProtobuf: MenuDoc is Protobuf.Parsable =
    locomotion.Inlinable.parsable[MenuDoc]

  given inlinedBintel: MenuDoc is Bintel.Parsable = BintelInlinable.parsable[MenuDoc]

  def jsonAst(): MenuDoc = json.read[Json].as[MenuDoc]
  def jsonInlined(): MenuDoc = json.read[MenuDoc in Json]
  def telAst(): MenuDoc = tel.read[Tel].as[MenuDoc]
  def telInlined(): MenuDoc = tel.read[MenuDoc in Tel]
  def xmlAst(): MenuDoc = xml.read[Xml].as[MenuDoc]
  def xmlInlined(): MenuDoc = xml.read[MenuDoc in Xml]
  def yamlAst(): MenuDoc = yaml.read[Yaml].as[MenuDoc]
  def cborAst(): MenuDoc = cbor.read[Cbor].as[MenuDoc]
  def cborInlined(): MenuDoc = cbor.read[MenuDoc in Cbor]
  def protobufAst(): MenuDoc = protobuf.read[Protobuf].as[MenuDoc]
  def protobufInlined(): MenuDoc = protobuf.read[MenuDoc in Protobuf]
  def bintelAst(): MenuDoc = Bintel.read[MenuDoc](bintel)
  def bintelInlined(): MenuDoc = Bintel.parse[MenuDoc](bintel)

  // ── The third-party baselines ──
  def jsoniterRival(): MMenuDoc =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[MMenuDoc]
      (json.mutable(using Unsafe))(using jsoniterCodecs.menu)

  def borerRival(): MMenuDoc =
    io.bullet.borer.Cbor.decode(cbor.mutable(using Unsafe)).to[MMenuDoc]
      (using borerCodecs.menu).value

  def protobufJavaRival(): MMenuDoc = protobufWalks.menu(protobuf)
  def aaltoRival(): MMenuDoc = aaltoWalks.menu(xml.s)
  def snakeyamlRival(): MMenuDoc = snakeyamlWalks.menu(yaml.s)

  // The correctness gate: every arm must reproduce the corpus before
  // anything is timed.
  def check(): Unit =
    assert(jsonInlined() == corpus, "menu: JSON inlined decode disagrees")
    assert(jsonAst() == corpus, "menu: JSON AST decode disagrees")
    assert(telInlined() == corpus, "menu: TEL inlined decode disagrees")
    assert(telAst() == corpus, "menu: TEL AST decode disagrees")
    assert(xmlInlined() == corpus, "menu: XML inlined decode disagrees")
    assert(xmlAst() == corpus, "menu: XML AST decode disagrees")
    assert(yamlAst() == corpus, "menu: YAML AST decode disagrees")
    assert(cborInlined() == corpus, "menu: CBOR inlined decode disagrees")
    assert(cborAst() == corpus, "menu: CBOR AST decode disagrees")
    assert(protobufInlined() == corpus, "menu: Protobuf inlined decode disagrees")
    assert(protobufAst() == corpus, "menu: Protobuf AST decode disagrees")
    assert(bintelInlined() == corpus, "menu: BinTEL inlined decode disagrees")
    assert(bintelAst() == corpus, "menu: BinTEL AST decode disagrees")

    val mirror = mirrors.menu(corpus)
    assert(jsoniterRival() == mirror, "menu: Jsoniter decode disagrees")
    assert(borerRival() == mirror, "menu: borer decode disagrees")
    assert(protobufJavaRival() == mirror, "menu: protobuf-java decode disagrees")
    assert(aaltoRival() == mirror, "menu: Aalto decode disagrees")
    assert(snakeyamlRival() == mirror, "menu: snakeyaml decode disagrees")

  def suites[report](bench: Bench)
    (using Runner[report], Inclusion[report, Benchmark], Testable)
  :   Unit =

    suite(m"Decode the menu corpus to case classes"):
      bench(m"JSON inlined")(target = 1*Second):
        '{ crossparse.matrixMenu.jsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.matrixMenu.jsonAst() }

      bench(m"Jsoniter (JSON)")(target = 1*Second):
        '{ crossparse.matrixMenu.jsoniterRival() }

      bench(m"TEL inlined")(target = 1*Second):
        '{ crossparse.matrixMenu.telInlined() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.matrixMenu.telAst() }

      bench(m"XML inlined")(target = 1*Second):
        '{ crossparse.matrixMenu.xmlInlined() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.matrixMenu.xmlAst() }

      bench(m"Aalto (XML)")(target = 1*Second):
        '{ crossparse.matrixMenu.aaltoRival() }

      bench(m"CBOR inlined")(target = 1*Second):
        '{ crossparse.matrixMenu.cborInlined() }

      bench(m"CBOR via AST")(target = 1*Second):
        '{ crossparse.matrixMenu.cborAst() }

      bench(m"borer (CBOR)")(target = 1*Second):
        '{ crossparse.matrixMenu.borerRival() }

      bench(m"Protobuf inlined")(target = 1*Second):
        '{ crossparse.matrixMenu.protobufInlined() }

      bench(m"Protobuf via AST")(target = 1*Second):
        '{ crossparse.matrixMenu.protobufAst() }

      bench(m"protobuf-java")(target = 1*Second):
        '{ crossparse.matrixMenu.protobufJavaRival() }

      bench(m"BinTEL inlined")(target = 1*Second):
        '{ crossparse.matrixMenu.bintelInlined() }

      bench(m"BinTEL via AST")(target = 1*Second):
        '{ crossparse.matrixMenu.bintelAst() }

      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.matrixMenu.yamlAst() }

      bench(m"SnakeYAML (YAML)")(target = 1*Second):
        '{ crossparse.matrixMenu.snakeyamlRival() }

object matrixUsers:
  given schema: XmlSchema = XmlSchema.Freeform

  val corpus: Users = corpora.users()

  lazy val json: Data = corpus.in[Json].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val tel: Data = corpus.in[Tel].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val xml: Text = corpus.in[Xml].show
  lazy val yaml: Text = corpus.in[Yaml].show
  lazy val cbor: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(corpus.in[Cbor]))
  lazy val protobuf: Data = corpus.in[Protobuf].encode
  lazy val bintel: Data = corpus.bintel

  given inlinedJson: Users is Json.Parsable = jacinta.Inlinable.parsable[Users]
  given inlinedTel: Users is Tel.Parsable = stratiform.Inlinable.parsable[Users]
  given inlinedXml: Users is Xml.Parsable = xylophone.Inlinable.parsable[Users]
  given inlinedCbor: Users is Cbor.Parsable = breviloquence.Inlinable.parsable[Users]

  given inlinedProtobuf: Users is Protobuf.Parsable =
    locomotion.Inlinable.parsable[Users]

  given inlinedBintel: Users is Bintel.Parsable = BintelInlinable.parsable[Users]

  def jsonAst(): Users = json.read[Json].as[Users]
  def jsonInlined(): Users = json.read[Users in Json]
  def telAst(): Users = tel.read[Tel].as[Users]
  def telInlined(): Users = tel.read[Users in Tel]
  def xmlAst(): Users = xml.read[Xml].as[Users]
  def xmlInlined(): Users = xml.read[Users in Xml]
  def yamlAst(): Users = yaml.read[Yaml].as[Users]
  def cborAst(): Users = cbor.read[Cbor].as[Users]
  def cborInlined(): Users = cbor.read[Users in Cbor]
  def protobufAst(): Users = protobuf.read[Protobuf].as[Users]
  def protobufInlined(): Users = protobuf.read[Users in Protobuf]
  def bintelAst(): Users = Bintel.read[Users](bintel)
  def bintelInlined(): Users = Bintel.parse[Users](bintel)

  // ── The third-party baselines ──
  def jsoniterRival(): MUsers =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[MUsers]
      (json.mutable(using Unsafe))(using jsoniterCodecs.users)

  def borerRival(): MUsers =
    io.bullet.borer.Cbor.decode(cbor.mutable(using Unsafe)).to[MUsers]
      (using borerCodecs.users).value

  def protobufJavaRival(): MUsers = protobufWalks.users(protobuf)
  def aaltoRival(): MUsers = aaltoWalks.users(xml.s)
  def snakeyamlRival(): MUsers = snakeyamlWalks.users(yaml.s)

  // The correctness gate: every arm must reproduce the corpus before
  // anything is timed.
  def check(): Unit =
    assert(jsonInlined() == corpus, "users: JSON inlined decode disagrees")
    assert(jsonAst() == corpus, "users: JSON AST decode disagrees")
    assert(telInlined() == corpus, "users: TEL inlined decode disagrees")
    assert(telAst() == corpus, "users: TEL AST decode disagrees")
    assert(xmlInlined() == corpus, "users: XML inlined decode disagrees")
    assert(xmlAst() == corpus, "users: XML AST decode disagrees")
    assert(yamlAst() == corpus, "users: YAML AST decode disagrees")
    assert(cborInlined() == corpus, "users: CBOR inlined decode disagrees")
    assert(cborAst() == corpus, "users: CBOR AST decode disagrees")
    assert(protobufInlined() == corpus, "users: Protobuf inlined decode disagrees")
    assert(protobufAst() == corpus, "users: Protobuf AST decode disagrees")
    assert(bintelInlined() == corpus, "users: BinTEL inlined decode disagrees")
    assert(bintelAst() == corpus, "users: BinTEL AST decode disagrees")

    val mirror = mirrors.users(corpus)
    assert(jsoniterRival() == mirror, "users: Jsoniter decode disagrees")
    assert(borerRival() == mirror, "users: borer decode disagrees")
    assert(protobufJavaRival() == mirror, "users: protobuf-java decode disagrees")
    assert(aaltoRival() == mirror, "users: Aalto decode disagrees")
    assert(snakeyamlRival() == mirror, "users: snakeyaml decode disagrees")

  def suites[report](bench: Bench)
    (using Runner[report], Inclusion[report, Benchmark], Testable)
  :   Unit =

    suite(m"Decode the users corpus to case classes"):
      bench(m"JSON inlined")(target = 1*Second):
        '{ crossparse.matrixUsers.jsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.matrixUsers.jsonAst() }

      bench(m"Jsoniter (JSON)")(target = 1*Second):
        '{ crossparse.matrixUsers.jsoniterRival() }

      bench(m"TEL inlined")(target = 1*Second):
        '{ crossparse.matrixUsers.telInlined() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.matrixUsers.telAst() }

      bench(m"XML inlined")(target = 1*Second):
        '{ crossparse.matrixUsers.xmlInlined() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.matrixUsers.xmlAst() }

      bench(m"Aalto (XML)")(target = 1*Second):
        '{ crossparse.matrixUsers.aaltoRival() }

      bench(m"CBOR inlined")(target = 1*Second):
        '{ crossparse.matrixUsers.cborInlined() }

      bench(m"CBOR via AST")(target = 1*Second):
        '{ crossparse.matrixUsers.cborAst() }

      bench(m"borer (CBOR)")(target = 1*Second):
        '{ crossparse.matrixUsers.borerRival() }

      bench(m"Protobuf inlined")(target = 1*Second):
        '{ crossparse.matrixUsers.protobufInlined() }

      bench(m"Protobuf via AST")(target = 1*Second):
        '{ crossparse.matrixUsers.protobufAst() }

      bench(m"protobuf-java")(target = 1*Second):
        '{ crossparse.matrixUsers.protobufJavaRival() }

      bench(m"BinTEL inlined")(target = 1*Second):
        '{ crossparse.matrixUsers.bintelInlined() }

      bench(m"BinTEL via AST")(target = 1*Second):
        '{ crossparse.matrixUsers.bintelAst() }

      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.matrixUsers.yamlAst() }

      bench(m"SnakeYAML (YAML)")(target = 1*Second):
        '{ crossparse.matrixUsers.snakeyamlRival() }

object matrixLogs:
  given schema: XmlSchema = XmlSchema.Freeform

  val corpus: Logs = corpora.logs()

  lazy val json: Data = corpus.in[Json].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val tel: Data = corpus.in[Tel].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val xml: Text = corpus.in[Xml].show
  lazy val yaml: Text = corpus.in[Yaml].show
  lazy val cbor: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(corpus.in[Cbor]))
  lazy val protobuf: Data = corpus.in[Protobuf].encode
  lazy val bintel: Data = corpus.bintel

  given inlinedJson: Logs is Json.Parsable = jacinta.Inlinable.parsable[Logs]
  given inlinedTel: Logs is Tel.Parsable = stratiform.Inlinable.parsable[Logs]
  given inlinedXml: Logs is Xml.Parsable = xylophone.Inlinable.parsable[Logs]
  given inlinedCbor: Logs is Cbor.Parsable = breviloquence.Inlinable.parsable[Logs]

  given inlinedProtobuf: Logs is Protobuf.Parsable =
    locomotion.Inlinable.parsable[Logs]

  given inlinedBintel: Logs is Bintel.Parsable = BintelInlinable.parsable[Logs]

  def jsonAst(): Logs = json.read[Json].as[Logs]
  def jsonInlined(): Logs = json.read[Logs in Json]
  def telAst(): Logs = tel.read[Tel].as[Logs]
  def telInlined(): Logs = tel.read[Logs in Tel]
  def xmlAst(): Logs = xml.read[Xml].as[Logs]
  def xmlInlined(): Logs = xml.read[Logs in Xml]
  def yamlAst(): Logs = yaml.read[Yaml].as[Logs]
  def cborAst(): Logs = cbor.read[Cbor].as[Logs]
  def cborInlined(): Logs = cbor.read[Logs in Cbor]
  def protobufAst(): Logs = protobuf.read[Protobuf].as[Logs]
  def protobufInlined(): Logs = protobuf.read[Logs in Protobuf]
  def bintelAst(): Logs = Bintel.read[Logs](bintel)
  def bintelInlined(): Logs = Bintel.parse[Logs](bintel)

  // ── The third-party baselines ──
  def jsoniterRival(): MLogs =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[MLogs]
      (json.mutable(using Unsafe))(using jsoniterCodecs.logs)

  def borerRival(): MLogs =
    io.bullet.borer.Cbor.decode(cbor.mutable(using Unsafe)).to[MLogs]
      (using borerCodecs.logs).value

  def protobufJavaRival(): MLogs = protobufWalks.logs(protobuf)
  def aaltoRival(): MLogs = aaltoWalks.logs(xml.s)
  def snakeyamlRival(): MLogs = snakeyamlWalks.logs(yaml.s)

  // The correctness gate: every arm must reproduce the corpus before
  // anything is timed.
  def check(): Unit =
    assert(jsonInlined() == corpus, "logs: JSON inlined decode disagrees")
    assert(jsonAst() == corpus, "logs: JSON AST decode disagrees")
    assert(telInlined() == corpus, "logs: TEL inlined decode disagrees")
    assert(telAst() == corpus, "logs: TEL AST decode disagrees")
    assert(xmlInlined() == corpus, "logs: XML inlined decode disagrees")
    assert(xmlAst() == corpus, "logs: XML AST decode disagrees")
    assert(yamlAst() == corpus, "logs: YAML AST decode disagrees")
    assert(cborInlined() == corpus, "logs: CBOR inlined decode disagrees")
    assert(cborAst() == corpus, "logs: CBOR AST decode disagrees")
    assert(protobufInlined() == corpus, "logs: Protobuf inlined decode disagrees")
    assert(protobufAst() == corpus, "logs: Protobuf AST decode disagrees")
    assert(bintelInlined() == corpus, "logs: BinTEL inlined decode disagrees")
    assert(bintelAst() == corpus, "logs: BinTEL AST decode disagrees")

    val mirror = mirrors.logs(corpus)
    assert(jsoniterRival() == mirror, "logs: Jsoniter decode disagrees")
    assert(borerRival() == mirror, "logs: borer decode disagrees")
    assert(protobufJavaRival() == mirror, "logs: protobuf-java decode disagrees")
    assert(aaltoRival() == mirror, "logs: Aalto decode disagrees")
    assert(snakeyamlRival() == mirror, "logs: snakeyaml decode disagrees")

  def suites[report](bench: Bench)
    (using Runner[report], Inclusion[report, Benchmark], Testable)
  :   Unit =

    suite(m"Decode the logs corpus to case classes"):
      bench(m"JSON inlined")(target = 1*Second):
        '{ crossparse.matrixLogs.jsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.matrixLogs.jsonAst() }

      bench(m"Jsoniter (JSON)")(target = 1*Second):
        '{ crossparse.matrixLogs.jsoniterRival() }

      bench(m"TEL inlined")(target = 1*Second):
        '{ crossparse.matrixLogs.telInlined() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.matrixLogs.telAst() }

      bench(m"XML inlined")(target = 1*Second):
        '{ crossparse.matrixLogs.xmlInlined() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.matrixLogs.xmlAst() }

      bench(m"Aalto (XML)")(target = 1*Second):
        '{ crossparse.matrixLogs.aaltoRival() }

      bench(m"CBOR inlined")(target = 1*Second):
        '{ crossparse.matrixLogs.cborInlined() }

      bench(m"CBOR via AST")(target = 1*Second):
        '{ crossparse.matrixLogs.cborAst() }

      bench(m"borer (CBOR)")(target = 1*Second):
        '{ crossparse.matrixLogs.borerRival() }

      bench(m"Protobuf inlined")(target = 1*Second):
        '{ crossparse.matrixLogs.protobufInlined() }

      bench(m"Protobuf via AST")(target = 1*Second):
        '{ crossparse.matrixLogs.protobufAst() }

      bench(m"protobuf-java")(target = 1*Second):
        '{ crossparse.matrixLogs.protobufJavaRival() }

      bench(m"BinTEL inlined")(target = 1*Second):
        '{ crossparse.matrixLogs.bintelInlined() }

      bench(m"BinTEL via AST")(target = 1*Second):
        '{ crossparse.matrixLogs.bintelAst() }

      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.matrixLogs.yamlAst() }

      bench(m"SnakeYAML (YAML)")(target = 1*Second):
        '{ crossparse.matrixLogs.snakeyamlRival() }

object matrixTransactions:
  given schema: XmlSchema = XmlSchema.Freeform

  val corpus: Transactions = corpora.transactions()

  lazy val json: Data = corpus.in[Json].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val tel: Data = corpus.in[Tel].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val xml: Text = corpus.in[Xml].show
  lazy val yaml: Text = corpus.in[Yaml].show
  lazy val cbor: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(corpus.in[Cbor]))
  lazy val protobuf: Data = corpus.in[Protobuf].encode
  lazy val bintel: Data = corpus.bintel

  given inlinedJson: Transactions is Json.Parsable = jacinta.Inlinable.parsable[Transactions]
  given inlinedTel: Transactions is Tel.Parsable = stratiform.Inlinable.parsable[Transactions]
  given inlinedXml: Transactions is Xml.Parsable = xylophone.Inlinable.parsable[Transactions]
  given inlinedCbor: Transactions is Cbor.Parsable = breviloquence.Inlinable.parsable[Transactions]

  given inlinedProtobuf: Transactions is Protobuf.Parsable =
    locomotion.Inlinable.parsable[Transactions]

  given inlinedBintel: Transactions is Bintel.Parsable = BintelInlinable.parsable[Transactions]

  def jsonAst(): Transactions = json.read[Json].as[Transactions]
  def jsonInlined(): Transactions = json.read[Transactions in Json]
  def telAst(): Transactions = tel.read[Tel].as[Transactions]
  def telInlined(): Transactions = tel.read[Transactions in Tel]
  def xmlAst(): Transactions = xml.read[Xml].as[Transactions]
  def xmlInlined(): Transactions = xml.read[Transactions in Xml]
  def yamlAst(): Transactions = yaml.read[Yaml].as[Transactions]
  def cborAst(): Transactions = cbor.read[Cbor].as[Transactions]
  def cborInlined(): Transactions = cbor.read[Transactions in Cbor]
  def protobufAst(): Transactions = protobuf.read[Protobuf].as[Transactions]
  def protobufInlined(): Transactions = protobuf.read[Transactions in Protobuf]
  def bintelAst(): Transactions = Bintel.read[Transactions](bintel)
  def bintelInlined(): Transactions = Bintel.parse[Transactions](bintel)

  // ── The third-party baselines ──
  def jsoniterRival(): MTransactions =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[MTransactions]
      (json.mutable(using Unsafe))(using jsoniterCodecs.transactions)

  def borerRival(): MTransactions =
    io.bullet.borer.Cbor.decode(cbor.mutable(using Unsafe)).to[MTransactions]
      (using borerCodecs.transactions).value

  def protobufJavaRival(): MTransactions = protobufWalks.transactions(protobuf)
  def aaltoRival(): MTransactions = aaltoWalks.transactions(xml.s)
  def snakeyamlRival(): MTransactions = snakeyamlWalks.transactions(yaml.s)

  // The correctness gate: every arm must reproduce the corpus before
  // anything is timed.
  def check(): Unit =
    assert(jsonInlined() == corpus, "transactions: JSON inlined decode disagrees")
    assert(jsonAst() == corpus, "transactions: JSON AST decode disagrees")
    assert(telInlined() == corpus, "transactions: TEL inlined decode disagrees")
    assert(telAst() == corpus, "transactions: TEL AST decode disagrees")
    assert(xmlInlined() == corpus, "transactions: XML inlined decode disagrees")
    assert(xmlAst() == corpus, "transactions: XML AST decode disagrees")
    assert(yamlAst() == corpus, "transactions: YAML AST decode disagrees")
    assert(cborInlined() == corpus, "transactions: CBOR inlined decode disagrees")
    assert(cborAst() == corpus, "transactions: CBOR AST decode disagrees")
    assert(protobufInlined() == corpus, "transactions: Protobuf inlined decode disagrees")
    assert(protobufAst() == corpus, "transactions: Protobuf AST decode disagrees")
    assert(bintelInlined() == corpus, "transactions: BinTEL inlined decode disagrees")
    assert(bintelAst() == corpus, "transactions: BinTEL AST decode disagrees")

    val mirror = mirrors.transactions(corpus)
    assert(jsoniterRival() == mirror, "transactions: Jsoniter decode disagrees")
    assert(borerRival() == mirror, "transactions: borer decode disagrees")
    assert(protobufJavaRival() == mirror, "transactions: protobuf-java decode disagrees")
    assert(aaltoRival() == mirror, "transactions: Aalto decode disagrees")
    assert(snakeyamlRival() == mirror, "transactions: snakeyaml decode disagrees")

  def suites[report](bench: Bench)
    (using Runner[report], Inclusion[report, Benchmark], Testable)
  :   Unit =

    suite(m"Decode the transactions corpus to case classes"):
      bench(m"JSON inlined")(target = 1*Second):
        '{ crossparse.matrixTransactions.jsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.matrixTransactions.jsonAst() }

      bench(m"Jsoniter (JSON)")(target = 1*Second):
        '{ crossparse.matrixTransactions.jsoniterRival() }

      bench(m"TEL inlined")(target = 1*Second):
        '{ crossparse.matrixTransactions.telInlined() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.matrixTransactions.telAst() }

      bench(m"XML inlined")(target = 1*Second):
        '{ crossparse.matrixTransactions.xmlInlined() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.matrixTransactions.xmlAst() }

      bench(m"Aalto (XML)")(target = 1*Second):
        '{ crossparse.matrixTransactions.aaltoRival() }

      bench(m"CBOR inlined")(target = 1*Second):
        '{ crossparse.matrixTransactions.cborInlined() }

      bench(m"CBOR via AST")(target = 1*Second):
        '{ crossparse.matrixTransactions.cborAst() }

      bench(m"borer (CBOR)")(target = 1*Second):
        '{ crossparse.matrixTransactions.borerRival() }

      bench(m"Protobuf inlined")(target = 1*Second):
        '{ crossparse.matrixTransactions.protobufInlined() }

      bench(m"Protobuf via AST")(target = 1*Second):
        '{ crossparse.matrixTransactions.protobufAst() }

      bench(m"protobuf-java")(target = 1*Second):
        '{ crossparse.matrixTransactions.protobufJavaRival() }

      bench(m"BinTEL inlined")(target = 1*Second):
        '{ crossparse.matrixTransactions.bintelInlined() }

      bench(m"BinTEL via AST")(target = 1*Second):
        '{ crossparse.matrixTransactions.bintelAst() }

      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.matrixTransactions.yamlAst() }

      bench(m"SnakeYAML (YAML)")(target = 1*Second):
        '{ crossparse.matrixTransactions.snakeyamlRival() }

object matrixInts:
  given schema: XmlSchema = XmlSchema.Freeform

  val corpus: Ints = corpora.ints()

  lazy val json: Data = corpus.in[Json].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val tel: Data = corpus.in[Tel].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val xml: Text = corpus.in[Xml].show
  lazy val yaml: Text = corpus.in[Yaml].show
  lazy val cbor: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(corpus.in[Cbor]))
  lazy val protobuf: Data = corpus.in[Protobuf].encode
  lazy val bintel: Data = corpus.bintel

  given inlinedJson: Ints is Json.Parsable = jacinta.Inlinable.parsable[Ints]
  given inlinedTel: Ints is Tel.Parsable = stratiform.Inlinable.parsable[Ints]
  given inlinedXml: Ints is Xml.Parsable = xylophone.Inlinable.parsable[Ints]
  given inlinedCbor: Ints is Cbor.Parsable = breviloquence.Inlinable.parsable[Ints]

  given inlinedProtobuf: Ints is Protobuf.Parsable =
    locomotion.Inlinable.parsable[Ints]

  given inlinedBintel: Ints is Bintel.Parsable = BintelInlinable.parsable[Ints]

  def jsonAst(): Ints = json.read[Json].as[Ints]
  def jsonInlined(): Ints = json.read[Ints in Json]
  def telAst(): Ints = tel.read[Tel].as[Ints]
  def telInlined(): Ints = tel.read[Ints in Tel]
  def xmlAst(): Ints = xml.read[Xml].as[Ints]
  def xmlInlined(): Ints = xml.read[Ints in Xml]
  def yamlAst(): Ints = yaml.read[Yaml].as[Ints]
  def cborAst(): Ints = cbor.read[Cbor].as[Ints]
  def cborInlined(): Ints = cbor.read[Ints in Cbor]
  def protobufAst(): Ints = protobuf.read[Protobuf].as[Ints]
  def protobufInlined(): Ints = protobuf.read[Ints in Protobuf]
  def bintelAst(): Ints = Bintel.read[Ints](bintel)
  def bintelInlined(): Ints = Bintel.parse[Ints](bintel)

  // ── The third-party baselines ──
  def jsoniterRival(): MInts =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[MInts]
      (json.mutable(using Unsafe))(using jsoniterCodecs.ints)

  def borerRival(): MInts =
    io.bullet.borer.Cbor.decode(cbor.mutable(using Unsafe)).to[MInts]
      (using borerCodecs.ints).value

  def protobufJavaRival(): MInts = protobufWalks.ints(protobuf)
  def aaltoRival(): MInts = aaltoWalks.ints(xml.s)
  def snakeyamlRival(): MInts = snakeyamlWalks.ints(yaml.s)

  // The correctness gate: every arm must reproduce the corpus before
  // anything is timed.
  def check(): Unit =
    assert(jsonInlined() == corpus, "ints: JSON inlined decode disagrees")
    assert(jsonAst() == corpus, "ints: JSON AST decode disagrees")
    assert(telInlined() == corpus, "ints: TEL inlined decode disagrees")
    assert(telAst() == corpus, "ints: TEL AST decode disagrees")
    assert(xmlInlined() == corpus, "ints: XML inlined decode disagrees")
    assert(xmlAst() == corpus, "ints: XML AST decode disagrees")
    assert(yamlAst() == corpus, "ints: YAML AST decode disagrees")
    assert(cborInlined() == corpus, "ints: CBOR inlined decode disagrees")
    assert(cborAst() == corpus, "ints: CBOR AST decode disagrees")
    assert(protobufInlined() == corpus, "ints: Protobuf inlined decode disagrees")
    assert(protobufAst() == corpus, "ints: Protobuf AST decode disagrees")
    assert(bintelInlined() == corpus, "ints: BinTEL inlined decode disagrees")
    assert(bintelAst() == corpus, "ints: BinTEL AST decode disagrees")

    val mirror = mirrors.ints(corpus)
    assert(jsoniterRival() == mirror, "ints: Jsoniter decode disagrees")
    assert(borerRival() == mirror, "ints: borer decode disagrees")
    assert(protobufJavaRival() == mirror, "ints: protobuf-java decode disagrees")
    assert(aaltoRival() == mirror, "ints: Aalto decode disagrees")
    assert(snakeyamlRival() == mirror, "ints: snakeyaml decode disagrees")

  def suites[report](bench: Bench)
    (using Runner[report], Inclusion[report, Benchmark], Testable)
  :   Unit =

    suite(m"Decode the ints corpus to case classes"):
      bench(m"JSON inlined")(target = 1*Second):
        '{ crossparse.matrixInts.jsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.matrixInts.jsonAst() }

      bench(m"Jsoniter (JSON)")(target = 1*Second):
        '{ crossparse.matrixInts.jsoniterRival() }

      bench(m"TEL inlined")(target = 1*Second):
        '{ crossparse.matrixInts.telInlined() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.matrixInts.telAst() }

      bench(m"XML inlined")(target = 1*Second):
        '{ crossparse.matrixInts.xmlInlined() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.matrixInts.xmlAst() }

      bench(m"Aalto (XML)")(target = 1*Second):
        '{ crossparse.matrixInts.aaltoRival() }

      bench(m"CBOR inlined")(target = 1*Second):
        '{ crossparse.matrixInts.cborInlined() }

      bench(m"CBOR via AST")(target = 1*Second):
        '{ crossparse.matrixInts.cborAst() }

      bench(m"borer (CBOR)")(target = 1*Second):
        '{ crossparse.matrixInts.borerRival() }

      bench(m"Protobuf inlined")(target = 1*Second):
        '{ crossparse.matrixInts.protobufInlined() }

      bench(m"Protobuf via AST")(target = 1*Second):
        '{ crossparse.matrixInts.protobufAst() }

      bench(m"protobuf-java")(target = 1*Second):
        '{ crossparse.matrixInts.protobufJavaRival() }

      bench(m"BinTEL inlined")(target = 1*Second):
        '{ crossparse.matrixInts.bintelInlined() }

      bench(m"BinTEL via AST")(target = 1*Second):
        '{ crossparse.matrixInts.bintelAst() }

      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.matrixInts.yamlAst() }

      bench(m"SnakeYAML (YAML)")(target = 1*Second):
        '{ crossparse.matrixInts.snakeyamlRival() }

object matrixDecimals:
  given schema: XmlSchema = XmlSchema.Freeform

  val corpus: Decimals = corpora.decimals()

  lazy val json: Data = corpus.in[Json].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val tel: Data = corpus.in[Tel].show.s.getBytes("UTF-8").nn.immutable(using Unsafe)
  lazy val xml: Text = corpus.in[Xml].show
  lazy val yaml: Text = corpus.in[Yaml].show
  lazy val cbor: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(corpus.in[Cbor]))
  lazy val protobuf: Data = corpus.in[Protobuf].encode
  lazy val bintel: Data = corpus.bintel

  given inlinedJson: Decimals is Json.Parsable = jacinta.Inlinable.parsable[Decimals]
  given inlinedTel: Decimals is Tel.Parsable = stratiform.Inlinable.parsable[Decimals]
  given inlinedXml: Decimals is Xml.Parsable = xylophone.Inlinable.parsable[Decimals]
  given inlinedCbor: Decimals is Cbor.Parsable = breviloquence.Inlinable.parsable[Decimals]

  given inlinedProtobuf: Decimals is Protobuf.Parsable =
    locomotion.Inlinable.parsable[Decimals]

  given inlinedBintel: Decimals is Bintel.Parsable = BintelInlinable.parsable[Decimals]

  def jsonAst(): Decimals = json.read[Json].as[Decimals]
  def jsonInlined(): Decimals = json.read[Decimals in Json]
  def telAst(): Decimals = tel.read[Tel].as[Decimals]
  def telInlined(): Decimals = tel.read[Decimals in Tel]
  def xmlAst(): Decimals = xml.read[Xml].as[Decimals]
  def xmlInlined(): Decimals = xml.read[Decimals in Xml]
  def yamlAst(): Decimals = yaml.read[Yaml].as[Decimals]
  def cborAst(): Decimals = cbor.read[Cbor].as[Decimals]
  def cborInlined(): Decimals = cbor.read[Decimals in Cbor]
  def protobufAst(): Decimals = protobuf.read[Protobuf].as[Decimals]
  def protobufInlined(): Decimals = protobuf.read[Decimals in Protobuf]
  def bintelAst(): Decimals = Bintel.read[Decimals](bintel)
  def bintelInlined(): Decimals = Bintel.parse[Decimals](bintel)

  // ── The third-party baselines ──
  def jsoniterRival(): MDecimals =
    com.github.plokhotnyuk.jsoniter_scala.core.readFromArray[MDecimals]
      (json.mutable(using Unsafe))(using jsoniterCodecs.decimals)

  def borerRival(): MDecimals =
    io.bullet.borer.Cbor.decode(cbor.mutable(using Unsafe)).to[MDecimals]
      (using borerCodecs.decimals).value

  def protobufJavaRival(): MDecimals = protobufWalks.decimals(protobuf)
  def aaltoRival(): MDecimals = aaltoWalks.decimals(xml.s)
  def snakeyamlRival(): MDecimals = snakeyamlWalks.decimals(yaml.s)

  // The correctness gate: every arm must reproduce the corpus before
  // anything is timed.
  def check(): Unit =
    assert(jsonInlined() == corpus, "decimals: JSON inlined decode disagrees")
    assert(jsonAst() == corpus, "decimals: JSON AST decode disagrees")
    assert(telInlined() == corpus, "decimals: TEL inlined decode disagrees")
    assert(telAst() == corpus, "decimals: TEL AST decode disagrees")
    assert(xmlInlined() == corpus, "decimals: XML inlined decode disagrees")
    assert(xmlAst() == corpus, "decimals: XML AST decode disagrees")
    assert(yamlAst() == corpus, "decimals: YAML AST decode disagrees")
    assert(cborInlined() == corpus, "decimals: CBOR inlined decode disagrees")
    assert(cborAst() == corpus, "decimals: CBOR AST decode disagrees")
    assert(protobufInlined() == corpus, "decimals: Protobuf inlined decode disagrees")
    assert(protobufAst() == corpus, "decimals: Protobuf AST decode disagrees")
    assert(bintelInlined() == corpus, "decimals: BinTEL inlined decode disagrees")
    assert(bintelAst() == corpus, "decimals: BinTEL AST decode disagrees")

    val mirror = mirrors.decimals(corpus)
    assert(jsoniterRival() == mirror, "decimals: Jsoniter decode disagrees")
    assert(borerRival() == mirror, "decimals: borer decode disagrees")
    assert(protobufJavaRival() == mirror, "decimals: protobuf-java decode disagrees")
    assert(aaltoRival() == mirror, "decimals: Aalto decode disagrees")
    assert(snakeyamlRival() == mirror, "decimals: snakeyaml decode disagrees")

  def suites[report](bench: Bench)
    (using Runner[report], Inclusion[report, Benchmark], Testable)
  :   Unit =

    suite(m"Decode the decimals corpus to case classes"):
      bench(m"JSON inlined")(target = 1*Second):
        '{ crossparse.matrixDecimals.jsonInlined() }

      bench(m"JSON via AST")(target = 1*Second):
        '{ crossparse.matrixDecimals.jsonAst() }

      bench(m"Jsoniter (JSON)")(target = 1*Second):
        '{ crossparse.matrixDecimals.jsoniterRival() }

      bench(m"TEL inlined")(target = 1*Second):
        '{ crossparse.matrixDecimals.telInlined() }

      bench(m"TEL via AST")(target = 1*Second):
        '{ crossparse.matrixDecimals.telAst() }

      bench(m"XML inlined")(target = 1*Second):
        '{ crossparse.matrixDecimals.xmlInlined() }

      bench(m"XML via AST")(target = 1*Second):
        '{ crossparse.matrixDecimals.xmlAst() }

      bench(m"Aalto (XML)")(target = 1*Second):
        '{ crossparse.matrixDecimals.aaltoRival() }

      bench(m"CBOR inlined")(target = 1*Second):
        '{ crossparse.matrixDecimals.cborInlined() }

      bench(m"CBOR via AST")(target = 1*Second):
        '{ crossparse.matrixDecimals.cborAst() }

      bench(m"borer (CBOR)")(target = 1*Second):
        '{ crossparse.matrixDecimals.borerRival() }

      bench(m"Protobuf inlined")(target = 1*Second):
        '{ crossparse.matrixDecimals.protobufInlined() }

      bench(m"Protobuf via AST")(target = 1*Second):
        '{ crossparse.matrixDecimals.protobufAst() }

      bench(m"protobuf-java")(target = 1*Second):
        '{ crossparse.matrixDecimals.protobufJavaRival() }

      bench(m"BinTEL inlined")(target = 1*Second):
        '{ crossparse.matrixDecimals.bintelInlined() }

      bench(m"BinTEL via AST")(target = 1*Second):
        '{ crossparse.matrixDecimals.bintelAst() }

      bench(m"YAML via AST")(target = 1*Second):
        '{ crossparse.matrixDecimals.yamlAst() }

      bench(m"SnakeYAML (YAML)")(target = 1*Second):
        '{ crossparse.matrixDecimals.snakeyamlRival() }

// The deterministic corpus builders, mirroring the jacinta benchmark
// generators (adapted: text values avoid spaces and format-sensitive
// leading characters, so every format round-trips values byte-stably).
object corpora:
  def config(): Config =
    Config
      ( WebApp
          ( servlets = List
              ( Servlet
                  ( t"cofaxCDS", t"org.cofax.cds.CDSServlet",
                    List
                      ( Param(t"installationAt", t"Philadelphia-PA"),
                        Param(t"adminEmail", t"ksm@pobox.com"),
                        Param(t"poweredBy", t"Cofax"),
                        Param(t"poweredByIcon", t"/images/cofax.gif"),
                        Param(t"staticPath", t"/content/static"),
                        Param(t"templateProcessorClass", t"org.cofax.WysiwygTemplate"),
                        Param(t"templateLoaderClass", t"org.cofax.FilesTemplateLoader"),
                        Param(t"templatePath", t"templates") ) ),
                Servlet
                  ( t"cofaxEmail", t"org.cofax.cds.EmailServlet",
                    List
                      ( Param(t"mailHost", t"mail1"),
                        Param(t"mailHostOverride", t"mail2") ) ),
                Servlet(t"cofaxAdmin", t"org.cofax.cds.AdminServlet", Nil),
                Servlet(t"fileServlet", t"org.cofax.cds.FileServlet", Nil),
                Servlet
                  ( t"cofaxTools", t"org.cofax.cms.CofaxToolsServlet",
                    List
                      ( Param(t"templatePath", t"toolstemplates/"),
                        Param(t"logLocation", t"/usr/local/tomcat/logs/CofaxTools.log"),
                        Param(t"dataLog", t"1"),
                        Param(t"removePageCache", t"/content/admin/remove?cache=pages&id="),
                        Param(t"fileTransferFolder", t"/usr/local/tomcat/webapps/content"),
                        Param(t"lookInContext", t"1"),
                        Param(t"adminGroupID", t"4"),
                        Param(t"betaServer", t"true") ) ) ),
            mappings = List
              ( Mapping(t"cofaxCDS", t"/"),
                Mapping(t"cofaxEmail", t"/cofaxutil/aemail/*"),
                Mapping(t"cofaxAdmin", t"/admin/*"),
                Mapping(t"fileServlet", t"/static/*"),
                Mapping(t"cofaxTools", t"/tools/*") ),
            taglib = Taglib(t"cofax.tld", t"/WEB-INF/tlds/cofax.tld") ) )

  def menu(): MenuDoc =
    MenuDoc
      ( Menu
          ( t"file", t"File",
            Popup
              ( List
                  ( MenuItem(t"New", t"CreateNewDoc()"),
                    MenuItem(t"Open", t"OpenDoc()"),
                    MenuItem(t"Close", t"CloseDoc()") ) ) ) )

  def users(): Users =
    Users:
      List.tabulate(100): i =>
        User
          ( id       = i,
            username = t"user$i",
            email    = t"user$i@example.com",
            active   = (i & 1) == 0,
            role     = if i % 10 == 0 then t"admin" else t"user" )

  def logs(): Logs =
    val levels = List(t"info", t"debug", t"warn", t"error")
    val services = List(t"auth", t"api", t"db", t"cache", t"worker")

    Logs:
      List.tabulate(500): i =>
        LogEntry
          ( timestamp = 1700000000L + i,
            level     = levels(i & 3),
            service   = services(i % 5),
            requestId = t"req-$i",
            userId    = 1000 + (i % 50),
            message   = t"event-$i-processed" )

  def transactions(): Transactions =
    Transactions:
      List.tabulate(50): i =>
        Transaction
          ( from             = t"0xabcdef$i",
            to               = t"0x123456$i",
            valueWei         = t"12345678901234567890${1000 + i}",
            valueEth         = t"1234567890.12345678901${i % 10}",
            gasPriceWei      = t"30000000000.0123456789${i % 10}",
            gasUsed          = 21000 + i*100,
            blockNumber      = 18500000L + i,
            nonce            = i,
            temperatureDelta = (i % 10 + 1).toDouble/4096.0 )

  def ints(): Ints = Ints(List.tabulate(1000) { i => i*37 + 1 })

  // Fraction digits stay within every format's number fast path (a
  // ~13-significant-digit double exposes a jacinta AST compact-BCD
  // mis-decode — reported separately).
  def decimals(): Decimals =
    Decimals:
      List.tabulate(1000) { i => (i*7 + 1).toDouble + ((i*13 + 1) % 1000).toDouble/1000.0 }

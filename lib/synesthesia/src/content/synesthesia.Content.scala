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
package synesthesia

import anticipation.*
import contingency.*
import gossamer.*
import jacinta.*
import vacuous.*

// The content-block vocabulary shared by the Model Context Protocol and the Agent Client
// Protocol, which adopts MCP's JSON representations for content verbatim. It lives in its own
// component so that an ACP client (which spawns agents as subprocesses) can reuse these types
// without acquiring `synesthesia.core`'s HTTP server stack. `object Mcp` re-exports every member,
// so MCP code continues to read `Mcp.TextContent` and so on.
object Content:
  // Anchor givens for the records the hand-written sum codecs dispatch to via `.json`/`.as`.
  // Deriving each once here lets every consumer reference it instead of re-inline-expanding the
  // whole derivation. See the corresponding anchors in `object Mcp`.
  object Icon:
    given (Tactic[Json.Error]) => Icon is Json.Decodable =
      Json.DecodableDerivation.derived

    given Icon is Json.Encodable = Json.EncodableDerivation.derived

  object Annotations:
    given (Tactic[Json.Error]) => Annotations is Json.Decodable =
      Json.DecodableDerivation.derived

    given Annotations is Json.Encodable = Json.EncodableDerivation.derived

  object TextContent:
    given (Tactic[Json.Error]) => TextContent is Json.Decodable =
      Json.DecodableDerivation.derived

    given TextContent is Json.Encodable = Json.EncodableDerivation.derived

  object ImageContent:
    given (Tactic[Json.Error]) => ImageContent is Json.Decodable =
      Json.DecodableDerivation.derived

    given ImageContent is Json.Encodable = Json.EncodableDerivation.derived

  object AudioContent:
    given (Tactic[Json.Error]) => AudioContent is Json.Decodable =
      Json.DecodableDerivation.derived

    given AudioContent is Json.Encodable = Json.EncodableDerivation.derived

  object ResourceLink:
    given (Tactic[Json.Error]) => ResourceLink is Json.Decodable =
      Json.DecodableDerivation.derived

    given ResourceLink is Json.Encodable = Json.EncodableDerivation.derived

  object EmbeddedResource:
    given (Tactic[Json.Error]) => EmbeddedResource is Json.Decodable =
      Json.DecodableDerivation.derived

    given EmbeddedResource is Json.Encodable = Json.EncodableDerivation.derived

  object TextResourceContents:
    given (Tactic[Json.Error]) => TextResourceContents is Json.Decodable =
      Json.DecodableDerivation.derived

    given TextResourceContents is Json.Encodable = Json.EncodableDerivation.derived

  object BlobResourceContents:
    given (Tactic[Json.Error]) => BlobResourceContents is Json.Decodable =
      Json.DecodableDerivation.derived

    given BlobResourceContents is Json.Encodable = Json.EncodableDerivation.derived

  case class Icon
    ( src:      Text,
      mimeType: Optional[Text]       = Unset,
      sizes:    Optional[List[Text]] = Unset,
      theme:    Optional[Text]       = Unset )

  object Contents:
    given encodable: Contents is Json.Encodable = Json.Encodable(() => Morphology.Any):
      _.contents match
        case text: TextResourceContents => text.in[Json]
        case blob: BlobResourceContents => blob.in[Json]

    given decodable: Tactic[Json.Error] => Contents is Json.Decodable =
      Json.Decodable(Morphology.Any): json =>
        Contents(safely(json.as[TextResourceContents]).or(json.as[BlobResourceContents]))

  case class Contents(contents: TextResourceContents | BlobResourceContents)

  case class ResourceContents
    ( uri: Text, mimeType: Optional[Text] = Unset, _meta: Optional[Json] = Unset )

  case class TextResourceContents
    ( uri: Text, mimeType: Optional[Text] = Unset, text: Text, _meta: Optional[Json] = Unset )

  case class BlobResourceContents
    ( uri: Text, mimeType: Optional[Text] = Unset, blob: Text, _meta: Optional[Json] = Unset )

  case class Annotations
    ( audience:     Optional[List[Role]] = Unset,
      priority:     Optional[Double]     = Unset,
      lastModified: Optional[Text]       = Unset )

  object Role:
    given encodable: Role is Json.Encodable = Json.Encodable(() => Morphology.Str):
      case Role.User      => t"user".in[Json]
      case Role.Assistant => t"assistant".in[Json]

    given decodable: Tactic[Json.Error] => Role is Json.Decodable =
      Json.Decodable(Morphology.Str): json =>
        json.as[Text] match
          case t"user"      => Role.User
          case t"assistant" => Role.Assistant
          case _            => abort(Json.Error(Json.Error.Reason.OutOfRange))

  enum Role:
    case User, Assistant

  object ContentBlock:
    import dynamicAccess.dynamicJson

    private val typeTag = Json.discriminatedUnion[ContentBlock](t"type")

    given encodable: ContentBlock is Json.Encodable = Json.Encodable(() => Morphology.Any):
      case content: TextContent      => typeTag.rewrite(t"text",          content.in[Json])
      case content: ImageContent     => typeTag.rewrite(t"image",         content.in[Json])
      case content: AudioContent     => typeTag.rewrite(t"audio",         content.in[Json])
      case content: ResourceLink     => typeTag.rewrite(t"resource_link", content.in[Json])
      case content: EmbeddedResource => typeTag.rewrite(t"resource", content.in[Json])

    given decodable: Tactic[Json.Error] => ContentBlock is Json.Decodable =
      Json.Decodable(Morphology.Any): json =>
        json.`type`.as[Text] match
          case "text"          => json.as[TextContent]
          case "image"         => json.as[ImageContent]
          case "audio"         => json.as[AudioContent]
          case "resource_link" => json.as[ResourceLink]
          case "resource"      => json.as[EmbeddedResource]
          case _               => abort(Json.Error(Json.Error.Reason.OutOfRange))

  sealed trait ContentBlock

  case class TextContent(text: Text, annotations: Optional[Annotations] = Unset)
  extends ContentBlock

  // `uri` is defined by ACP (which otherwise adopts MCP's image content shape) and not by MCP;
  // as an `Optional` member it vanishes from the MCP wire when unset.
  case class ImageContent
    ( data:        Text,
      mimeType:    Text,
      uri:         Optional[Text]        = Unset,
      annotations: Optional[Annotations] = Unset )
  extends ContentBlock

  case class AudioContent(data: Text, mimeType: Text, annotations: Optional[Annotations] = Unset)
  extends ContentBlock

  case class ResourceLink
    ( name:        Text,
      uri:         Text,
      title:       Optional[Text]        = Unset,
      description: Optional[Text]        = Unset,
      icons:       Optional[List[Icon]]  = Unset,
      mimeType:    Optional[Text]        = Unset,
      annotations: Optional[Annotations] = Unset,
      size:        Optional[Long]        = Unset,
      _meta:       Optional[Json]        = Unset )
  extends ContentBlock

  case class EmbeddedResource
    ( resource:    Contents,
      annotations: Optional[Annotations] = Unset,
      _meta:       Optional[Json]        = Unset )
  extends ContentBlock

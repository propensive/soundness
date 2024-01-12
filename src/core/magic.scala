/*
    Gesticulate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gesticulate

import rudiments.*

import language.experimental.captureChecking

object Extensions:
  def guess(ext: String): MediaType = mediaTypes.getOrElse(ext, mediaTypes("bin"))

  val mediaTypes: Map[String, MediaType] = Map(
    "aac"    -> media"audio/aac",
    "abw"    -> media"application/x-abiword",
    "arc"    -> media"application/x-freearc",
    "avi"    -> media"video/x-msvideo",
    "azw"    -> media"application/vnd.amazon.ebook",
    "bin"    -> media"application/octet-stream",
    "bmp"    -> media"image/bmp",
    "bz"     -> media"application/x-bzip",
    "bz2"    -> media"application/x-bzip2",
    "cda"    -> media"application/x-cdf",
    "csh"    -> media"application/x-csh",
    "css"    -> media"text/css",
    "csv"    -> media"text/csv",
    "doc"    -> media"application/msword",
    "docx"   -> media"application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "eot"    -> media"application/vnd.ms-fontobject",
    "epub"   -> media"application/epub+zip",
    "gz"     -> media"application/gzip",
    "gif"    -> media"image/gif",
    "htm"    -> media"text/html",
    "html"   -> media"text/html",
    "ico"    -> media"image/vnd.microsoft.icon",
    "ics"    -> media"text/calendar",
    "jar"    -> media"application/x-java-archive",
    "jpeg"   -> media"image/jpeg",
    "jpg"    -> media"image/jpeg",
    "js"     -> media"text/javascript",
    "json"   -> media"application/json",
    "jsonld" -> media"application/ld+json",
    "mid"    -> media"audio/x-midi",
    "midi"   -> media"audio/x-midi",
    "mjs"    -> media"text/javascript",
    "mp3"    -> media"audio/mpeg",
    "mp4"    -> media"video/mp4",
    "mpeg"   -> media"video/mpeg",
    "mpkg"   -> media"application/vnd.apple.installer+xml",
    "odp"    -> media"application/vnd.oasis.opendocument.presentation",
    "ods"    -> media"application/vnd.oasis.opendocument.spreadsheet",
    "odt"    -> media"application/vnd.oasis.opendocument.text",
    "oga"    -> media"audio/ogg",
    "ogv"    -> media"video/ogg",
    "ogx"    -> media"application/ogg",
    "opus"   -> media"audio/opus",
    "otf"    -> media"font/otf",
    "png"    -> media"image/png",
    "pdf"    -> media"application/pdf",
    "php"    -> media"application/x-httpd-php",
    "ppt"    -> media"application/vnd.ms-powerpoint",
    "pptx"   -> media"application/vnd.openxmlformats-officedocument.presentationml.presentation",
    "rar"    -> media"application/vnd.rar",
    "rtf"    -> media"application/rtf",
    "sh"     -> media"application/x-sh",
    "svg"    -> media"image/svg+xml",
    "swf"    -> media"application/x-shockwave-flash",
    "tar"    -> media"application/x-tar",
    "tif"    -> media"image/tiff",
    "tiff"   -> media"image/tiff",
    "ts"     -> media"video/x-mp2t",
    "ttf"    -> media"font/ttf",
    "txt"    -> media"text/plain",
    "vsd"    -> media"application/vnd.visio",
    "wav"    -> media"audio/x-wav",
    "weba"   -> media"audio/x-webm",
    "webm"   -> media"video/x-webm",
    "webp"   -> media"image/x-webp",
    "woff"   -> media"font/woff",
    "woff2"  -> media"font/woff2",
    "xhtml"  -> media"application/xhtml+xml",
    "xls"    -> media"application/vnd.ms-excel",
    "xlsx"   -> media"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "xml"    -> media"application/xml",
    "xul"    -> media"application/vnd.mozilla.xul+xml",
    "zip"    -> media"application/zip",
    "3gp"    -> media"video/3gpp",
    "3g2"    -> media"video/3gpp2",
    "7z"     -> media"application/x-7z-compressed"
  )

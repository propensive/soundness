/*
    Gesticulate, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import gossamer.*

import language.experimental.captureChecking

object Extensions:
  def guess(ext: Text): MediaType = mediaTypes.getOrElse(ext, media"application/octet-stream")

  val mediaTypes: Map[Text, MediaType] = Map
   (t"aac"    -> media"audio/aac",
    t"abw"    -> media"application/x-abiword",
    t"arc"    -> media"application/x-freearc",
    t"avi"    -> media"video/x-msvideo",
    t"azw"    -> media"application/vnd.amazon.ebook",
    t"bin"    -> media"application/octet-stream",
    t"bmp"    -> media"image/bmp",
    t"bz"     -> media"application/x-bzip",
    t"bz2"    -> media"application/x-bzip2",
    t"cda"    -> media"application/x-cdf",
    t"csh"    -> media"application/x-csh",
    t"css"    -> media"text/css",
    t"csv"    -> media"text/csv",
    t"doc"    -> media"application/msword",
    t"docx"   -> media"application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    t"eot"    -> media"application/vnd.ms-fontobject",
    t"epub"   -> media"application/epub+zip",
    t"gz"     -> media"application/gzip",
    t"gif"    -> media"image/gif",
    t"htm"    -> media"text/html",
    t"html"   -> media"text/html",
    t"ico"    -> media"image/vnd.microsoft.icon",
    t"ics"    -> media"text/calendar",
    t"jar"    -> media"application/x-java-archive",
    t"jpeg"   -> media"image/jpeg",
    t"jpg"    -> media"image/jpeg",
    t"js"     -> media"text/javascript",
    t"json"   -> media"application/json",
    t"jsonld" -> media"application/ld+json",
    t"mid"    -> media"audio/x-midi",
    t"midi"   -> media"audio/x-midi",
    t"mjs"    -> media"text/javascript",
    t"mp3"    -> media"audio/mpeg",
    t"mp4"    -> media"video/mp4",
    t"mpeg"   -> media"video/mpeg",
    t"mpkg"   -> media"application/vnd.apple.installer+xml",
    t"odp"    -> media"application/vnd.oasis.opendocument.presentation",
    t"ods"    -> media"application/vnd.oasis.opendocument.spreadsheet",
    t"odt"    -> media"application/vnd.oasis.opendocument.text",
    t"oga"    -> media"audio/ogg",
    t"ogv"    -> media"video/ogg",
    t"ogx"    -> media"application/ogg",
    t"opus"   -> media"audio/opus",
    t"otf"    -> media"font/otf",
    t"png"    -> media"image/png",
    t"pdf"    -> media"application/pdf",
    t"php"    -> media"application/x-httpd-php",
    t"ppt"    -> media"application/vnd.ms-powerpoint",
    t"pptx"   -> media"application/vnd.openxmlformats-officedocument.presentationml.presentation",
    t"rar"    -> media"application/vnd.rar",
    t"rtf"    -> media"application/rtf",
    t"sh"     -> media"application/x-sh",
    t"svg"    -> media"image/svg+xml",
    t"swf"    -> media"application/x-shockwave-flash",
    t"tar"    -> media"application/x-tar",
    t"tif"    -> media"image/tiff",
    t"tiff"   -> media"image/tiff",
    t"ts"     -> media"video/x-mp2t",
    t"ttf"    -> media"font/ttf",
    t"txt"    -> media"text/plain",
    t"vsd"    -> media"application/vnd.visio",
    t"wav"    -> media"audio/x-wav",
    t"weba"   -> media"audio/x-webm",
    t"webm"   -> media"video/x-webm",
    t"webp"   -> media"image/x-webp",
    t"woff"   -> media"font/woff",
    t"woff2"  -> media"font/woff2",
    t"xhtml"  -> media"application/xhtml+xml",
    t"xls"    -> media"application/vnd.ms-excel",
    t"xlsx"   -> media"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    t"xml"    -> media"application/xml",
    t"xul"    -> media"application/vnd.mozilla.xul+xml",
    t"zip"    -> media"application/zip",
    t"3gp"    -> media"video/3gpp",
    t"3g2"    -> media"video/3gpp2",
    t"7z"     -> media"application/x-7z-compressed")

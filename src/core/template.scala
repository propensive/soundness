/*
    Honeycomb, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

// package honeycomb

// import vacuous.*
// import anticipation.*
// import hieroglyph.*
// import serpentine.*
// import nettlesome.*

// case class TwitterCard()

// def page
//     (title:       Text,
//      encoding:    Encoding              = enc"UTF-8",
//      description: Optional[Text]        = Unset,
//      subject:     Optional[Text]        = Unset,
//      keywords:    List[Text]            = Nil,
//      author:      Optional[Text]        = Unset,
//      viewport:    Optional[Text]        = Unset,
//      stylesheet:  Optional[SimplePath]  = Unset,
//      icon:        Unset.type            = Unset,
//      image:       Optional[SimplePath]  = Unset,
//      url:         Optional[HttpUrl]     = Unset,
//      metaType:    Unset.type            = Unset,
//      twitter:     Optional[TwitterCard] = Unset,
//      canonical:   Optional[HttpUrl]     = Unset,
//      alternative: Unset.type            = Unset,
//      manifest:    Optional[SimplePath]  = Unset,
//      touchIcon:   Optional[SimplePath]  = Unset,
//      rssFeed:     Optional[SimplePath]  = Unset,
//      atomFeed:    Optional[SimplePath]  = Unset)
//     (content: Body.Children*): HtmlDoc =
//   HtmlDoc(Html
//    (Head
//      (Title(title)),
//     Body(content*)))

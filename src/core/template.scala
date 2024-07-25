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

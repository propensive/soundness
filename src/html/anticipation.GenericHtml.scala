package anticipation

enum GenericHtml:
  case Text(text: Text)
  case Tag(name: Text, attributes: List[(Text, Text)], children: List[GenericHtml])

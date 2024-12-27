package anticipation

enum GenericHtml:
  case Textual(text: Text)
  case Node(name: Text, attributes: List[(Text, Text)], children: List[GenericHtml])

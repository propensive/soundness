package gossamer

import escapade.*

package defaultTextTypes:
  given ansiText: DefaultTextType { type TextType = AnsiText } = compiletime.erasedValue

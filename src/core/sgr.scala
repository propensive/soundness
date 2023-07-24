package escapade

object Escapade:
  opaque type Sgr = Long

//   extension (sgr: Sgr)
//     def fgRed: Int = ((sgr >>> 16) & 255).toInt
//     def fgGreen: Int = ((sgr >>> 8) & 255).toInt
//     def fgBlue: Int = (sgr & 255).toInt
    
//     def bgRed: Int = ((sgr >>> 40) & 255).toInt
//     def bgGreen: Int = ((sgr >>> 32) & 255).toInt
//     def bgBlue: Int = ((sgr >>> 24) & 255).toInt
    
//     def isBold: Boolean = (sgr & (1 << 48)) > 0
//     def isDim: Boolean = (sgr & (1 << 49)) > 0
//     def isItalic: Boolean = (sgr & (1 << 50)) > 0
//     def isUnderline: Boolean = (sgr & (1 << 51)) > 0
//     def isConcealed: Boolean = (sgr & (1 << 52)) > 0
//     def isStrike: Boolean = (sgr & (1 << 53)) > 0
    
//     def fgMask: Boolean = (sgr & (1 << 54)) > 0
//     def bgMask: Boolean = (sgr & (1 << 55)) > 0
//     def boldMask: Boolean = (sgr & (1 << 56)) > 0
//     def dimMask: Boolean = (sgr & (1 << 57)) > 0
//     def italicMask: Boolean = (sgr & (1 << 58)) > 0
//     def underlineMask: Boolean = (sgr & (1 << 59)) > 0
//     def concealedMask: Boolean = (sgr & (1 << 60)) > 0
//     def strikeMask: Boolean = (sgr & (1 << 61)) > 0
    
//     def changesRequired: Boolean = ((sgr >> 54) & 255) > 0
    
//     def mask = ((sgr >> 54) & 255)



//     def changes(current: Sgr, emit: String => Unit): Text =
//       var nonEmpty: Boolean = false
      
//       inline def add(value: Int) =
//         if nonEmpty then emit(';')
// 	emit(int.toString)
// 	nonEmpty = true

//       if 
      
//       if fgMask then
//         add(38)
// 	add(5)
// 	add(fgRed)
// 	add(fgGreen)
// 	add(fgBlue)

//       if bgMask then
//         add(48)
// 	add(5)
// 	add(bgRed)
// 	add(bgGreen)
// 	add(bgBlue)
      
//       if boldMask then add(if isBold then 1 else 22)
//       if dimMask then add(if isDim then 2 else 22)
//       if italicMask then add(if isItalic then 3 else 23)
//       if underlineMask then add(if isUnderline then 4 else 24)
//       if concealedMask then add(if isConcealed then 8 else 28)
//       if strikeMask then add(if isStrike then 9 else 29)

//       t"${27.toChar}[${buffer.toString}m"